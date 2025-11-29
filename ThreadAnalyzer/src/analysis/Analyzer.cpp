// analysis/Analyzer.cpp
#include "analysis/Analyzer.h"

#include <QSet>
#include <functional>

namespace analysis {

// ---------- 内部小工具：遍历 AST / 指针分类 ----------

namespace {

// 简单判断一个类型串是不是“指针类型”
static bool isPointerType(const QString &t)
{
    // 先用一个最粗暴的实现，后续你可以根据自己的 type 格式再细化
    return t.contains('*');
}

// 原始指针版遍历
static void walkAstRaw(const AstNode *node,
                       const std::function<void(const AstNode &)> &fn)
{
    if (!node) return;
    fn(*node);
    for (const auto &child : node->children) {
        walkAstRaw(child.data(), fn);
    }
}

// QSharedPointer 包装一层，外面依然可以用旧接口
static void walkAst(const QSharedPointer<AstNode> &node,
                    const std::function<void(const AstNode &)> &fn)
{
    walkAstRaw(node.data(), fn);
}

// Result 是你想要“每个结点返回的结果”类型
// F 的签名是：Result f(const AstNode &node, const QVector<Result> &childrenResults)
template<typename Result, typename F>
static Result foldAstRaw(const AstNode *node, const F &f)
{
    if (!node) {
        return Result{};  // 默认构造一个空结果
    }

    QVector<Result> childResults;
    childResults.reserve(node->children.size());
    for (const auto &ch : node->children) {
        childResults.push_back(foldAstRaw<Result>(ch.data(), f));
    }

    // 关键：每个结点在“拿到所有子结点的返回值之后”，由 f 来综合
    return f(*node, childResults);
}

template<typename Result, typename F>
static Result foldAst(const QSharedPointer<AstNode> &root, const F &f)
{
    return foldAstRaw<Result>(root.data(), f);
}

// ============= 整条语句的语义摘要（可随时扩展） =============
struct ExprSummary {
    bool hasPointerAssign   = false;  // 是否发生指针赋值/初始化
    bool hasDeref           = false;  // 是否出现 *p 或 p[i]
    bool hasAddrOf          = false;  // 是否出现 &x
    bool hasMalloc          = false;  // 是否出现 malloc/calloc/realloc
    bool hasNew             = false;  // 是否出现 new
    bool hasCall            = false;  // 是否出现函数调用
    bool hasCondOp          = false;  // 是否有 ?: 条件表达式
    bool hasBinaryOp        = false;  // 是否有二元操作符
    bool hasMemberAccess    = false;  // 是否有成员访问 p->x / p.x

    QSet<QString> usedVars;           // 所有出现的变量 symbolId
    QSet<QString> calledFuncs;        // 所有调用到的函数 symbolId
};



// 去掉最外层 cast / 括号
static const AstNode *stripCastsAndParens(const AstNode *n)
{
    if (!n) return nullptr;

    while (n &&
           (n->kind == "ImplicitCastExpr" ||
            n->kind == "CStyleCastExpr" ||
            n->kind == "ParenExpr"))
    {
        if (n->children.isEmpty())
            break;
        n = n->children.first().data();
    }
    return n;
}

// 粗略找一棵子树里的“第一个 DeclRefExpr”
static const AstNode *findFirstDeclRef(const AstNode *root)
{
    if (!root) return nullptr;

    const AstNode *result = nullptr;
    walkAstRaw(root, [&result](const AstNode &n) {
        if (!result && n.kind == "DeclRefExpr") {
            result = &n;
        }
    });
    return result;
}

// 只把真正的赋值运算符当成“赋值”
static bool isAssignmentOp(const QString &op)
{
    return op == "="  ||
           op == "+=" || op == "-=" ||
           op == "*=" || op == "/=" || op == "%=" ||
           op == "<<=" || op == ">>=" ||
           op == "&="  || op == "|=" || op == "^=";
}

// 找一棵子树里的“第一个指针变量引用”
static const AstNode *findFirstPtrRef(const AstNode *root)
{
    if (!root) return nullptr;

    const AstNode *result = nullptr;
    walkAstRaw(root, [&result](const AstNode &n) {
        if (!result &&
            n.kind == "DeclRefExpr" &&
            isPointerType(n.varType))
        {
            result = &n;
        }
    });
    return result;
}

// 找一棵子树里的“第一个指针变量引用（排除某个 symbolId，比如函数本身）”
static const AstNode *findFirstPtrRefExcluding(const AstNode *root,
                                               const QString &excludeSymbolId)
{
    if (!root) return nullptr;

    const AstNode *result = nullptr;
    walkAstRaw(root, [&result, &excludeSymbolId](const AstNode &n) {
        if (!result &&
            n.kind == "DeclRefExpr" &&
            isPointerType(n.varType) &&
            (excludeSymbolId.isEmpty() || n.symbolId != excludeSymbolId))
        {
            result = &n;
        }
    });
    return result;
}

// 只在 LHS 顶层就是指针变量本身时才认为是“指针赋值/初始化”
static const AstNode *findPointerLHS(const AstNode &lhsRoot)
{
    const AstNode *n = stripCastsAndParens(&lhsRoot);
    if (!n) return nullptr;

    // 最常见：变量定义带初始化 / 普通赋值
    //   int *q = ...;
    //   q = ...;
    if ((n->kind == "VarLHS" || n->kind == "DeclRefExpr") &&
        isPointerType(n->varType)) {
        return n;
    }

    // 注意：不再往下递归找 first DeclRefExpr，
    // 这样 *q = 5; / p[i] = 5; 就不会被当成“给指针变量赋值”
    return nullptr;
}

// 指针赋值右侧的分类
enum class PtrAssignKind {
    Unknown,
    AddrOf,      // p = &x
    Null,        // p = nullptr / 0
    Copy,        // p = q
    FromMalloc,  // p = malloc(...)
    FromNew,     // p = new T(...)
    Conditional  // p = ...?...:...
};

// 对“指针右值表达式”的抽象结果
struct PtrExprInfo {
    PtrAssignKind kind = PtrAssignKind::Unknown;

    // 如果是 AddrOf / Copy，这里记录来源变量
    QString srcName;
    QString srcSymbolId;

    // 如果是条件表达式，可以记录一下是否混合多种来源（可选）
    bool fromConditional = false;
};

// 利用 foldAstRaw 对一棵 RHS 子树做“指针来源求值”
static PtrExprInfo evalPtrRhsExpr(const AstNode *root)
{
    return foldAstRaw<PtrExprInfo>(root,
                                   [](const AstNode &node, const QVector<PtrExprInfo> &children) {
                                       PtrExprInfo info;

                                       // 去掉最外层的 cast / 括号，但注意这里只是看“这个结点的语义”，
                                       // foldAst 的 children 是基于原始 children 算出来的，不受影响。
                                       const AstNode *n = stripCastsAndParens(&node);
                                       if (!n) return info;

                                       // 1) p = &x
                                       if (n->kind == "UnaryOperator" && n->op == "&") {
                                           if (!n->children.isEmpty()) {
                                               const AstNode *inner =
                                                   stripCastsAndParens(n->children.first().data());
                                               if (inner && inner->kind == "DeclRefExpr") {
                                                   info.kind        = PtrAssignKind::AddrOf;
                                                   info.srcName     = inner->varName;
                                                   info.srcSymbolId = inner->symbolId;
                                                   return info;
                                               }
                                           }
                                       }

                                       // 2) p = nullptr / 0
                                       if (n->kind == "CXXNullPtrLiteralExpr") {
                                           info.kind = PtrAssignKind::Null;
                                           return info;
                                       }
                                       if (n->kind == "IntegerLiteral" && isPointerType(n->varType)) {
                                           info.kind = PtrAssignKind::Null;
                                           return info;
                                       }

                                       // 3) p = q;  （q 也是指针）
                                       if (n->kind == "DeclRefExpr" && isPointerType(n->varType)) {
                                           info.kind        = PtrAssignKind::Copy;
                                           info.srcName     = n->varName;
                                           info.srcSymbolId = n->symbolId;
                                           return info;
                                       }

                                       // 4) p = malloc(...)
                                       if (n->kind == "CallExpr") {
                                           if (n->calleeName == "malloc" ||
                                               n->calleeName == "calloc" ||
                                               n->calleeName == "realloc")
                                           {
                                               info.kind = PtrAssignKind::FromMalloc;
                                               return info;
                                           }
                                       }

                                       // 5) p = new T(...)
                                       if (n->kind == "CXXNewExpr") {
                                           info.kind = PtrAssignKind::FromNew;
                                           return info;
                                       }

                                       // 6) 条件运算符：cond ? e1 : e2
                                       if (n->kind == "ConditionalOperator" && children.size() == 3) {
                                           // children[0] 是条件本身，一般对指针来源没用
                                           PtrExprInfo tcase = children[1];
                                           PtrExprInfo fcase = children[2];

                                           // 如果两边来源一致，就退化成同一种来源
                                           if (tcase.kind == fcase.kind &&
                                               tcase.kind != PtrAssignKind::Unknown &&
                                               tcase.srcSymbolId == fcase.srcSymbolId)
                                           {
                                               info = tcase;
                                               return info;
                                           }

                                           // 否则标记为“条件组合来源”，kind 暂时保持 Unknown，
                                           // 但 fromConditional = true，方便后续在 VarEvent 里加更具体信息
                                           info.kind            = PtrAssignKind::Conditional;
                                           info.fromConditional = true;

                                           // 你也可以选择在这里做更多合并策略，例如优先 malloc 等
                                           return info;
                                       }

                                       // 7) 其它结点：如果自己识别不了，就看看子结点有没有有意义的结果
                                       for (const PtrExprInfo &c : children) {
                                           if (c.kind != PtrAssignKind::Unknown || c.fromConditional) {
                                               return c;
                                           }
                                       }

                                       // 全部 Unknown，就保持默认
                                       return info;
                                   });
}

static ExprSummary summarizeStmtTree(const QSharedPointer<AstNode> &root)
{
    return foldAst<ExprSummary>(root,
                                [](const AstNode &n, const QVector<ExprSummary> &children) {
                                    ExprSummary s;

                                    // -------- 1) 先汇总子结点 --------
                                    for (const auto &c : children) {
                                        s.hasPointerAssign |= c.hasPointerAssign;
                                        s.hasDeref         |= c.hasDeref;
                                        s.hasAddrOf        |= c.hasAddrOf;
                                        s.hasMalloc        |= c.hasMalloc;
                                        s.hasNew           |= c.hasNew;
                                        s.hasCall          |= c.hasCall;
                                        s.hasCondOp        |= c.hasCondOp;
                                        s.hasBinaryOp      |= c.hasBinaryOp;
                                        s.hasMemberAccess  |= c.hasMemberAccess;

                                        s.usedVars.unite(c.usedVars);
                                        s.calledFuncs.unite(c.calledFuncs);
                                    }

                                    // -------- 2) 再看当前结点的语义信息 --------

                                    // 指针赋值：在分析器主逻辑中处理，不在这里专门识别
                                    if ((n.kind == "InitAssign") ||
                                        (n.kind == "BinaryOperator" && n.op == "="))
                                    {
                                        s.hasPointerAssign = true;
                                    }

                                    // 解引用
                                    if (n.kind == "UnaryOperator" && n.op == "*")
                                        s.hasDeref = true;
                                    if (n.kind == "ArraySubscriptExpr")
                                        s.hasDeref = true;

                                    // & 取地址
                                    if (n.kind == "UnaryOperator" && n.op == "&")
                                        s.hasAddrOf = true;

                                    // malloc / calloc / realloc
                                    if (n.kind == "CallExpr" &&
                                        (n.calleeName == "malloc" ||
                                         n.calleeName == "calloc" ||
                                         n.calleeName == "realloc"))
                                    {
                                        s.hasMalloc = true;
                                    }

                                    // new 表达式
                                    if (n.kind == "CXXNewExpr")
                                        s.hasNew = true;

                                    // 任意函数调用
                                    if (n.kind == "CallExpr") {
                                        s.hasCall = true;
                                        if (!n.calleeSymbolId.isEmpty())
                                            s.calledFuncs.insert(n.calleeSymbolId);
                                    }

                                    // ?: 条件表达式
                                    if (n.kind == "ConditionalOperator")
                                        s.hasCondOp = true;

                                    // 任意二元操作符
                                    if (n.kind == "BinaryOperator")
                                        s.hasBinaryOp = true;

                                    // p->x / p.x
                                    if (n.kind == "MemberExpr")
                                        s.hasMemberAccess = true;

                                    // 使用变量
                                    if (n.kind == "DeclRefExpr" && !n.symbolId.isEmpty())
                                        s.usedVars.insert(n.symbolId);

                                    return s;
                                });
}

// 对 RHS 表达式做分类
static PtrAssignKind classifyPointerRHS(const AstNode &rhsRoot,
                                        QString &srcName,
                                        QString &srcSymbolId)
{
    const AstNode *rhs = &rhsRoot;
    if (!rhs) return PtrAssignKind::Unknown;

    PtrExprInfo info = evalPtrRhsExpr(rhs);

    // 把 evalPtrRhsExpr 的结果映射回老接口
    srcName     = info.srcName;
    srcSymbolId = info.srcSymbolId;

    return info.kind;
}

// 一个“线程实例”：谁创建的哪个入口函数的线程
struct ThreadInstance {
    QString api;              // pthread_create / std::thread 等
    QString threadVarName;    // t1 / tid / ...
    QString startFuncName;    // 入口函数名
    QString startFuncSymbolId;// 入口函数 symbolId
};

// 入口 symbolId -> 线程实例列表
using ThreadMap = QHash<QString, QVector<ThreadInstance>>;

// 从 ProgramModel 里扫一遍，构建 ThreadMap
static ThreadMap buildThreadMap(const ProgramModel &model)
{
    ThreadMap m;

    for (const auto &f : model.functions) {
        for (const auto &b : f.blocks) {
            for (const auto &s : b.stmts) {
                for (const ThreadOp &op : s.threadOps) {
                    if (op.startFuncSymbolId.isEmpty())
                        continue;

                    ThreadInstance ti;
                    ti.api               = op.api;
                    ti.threadVarName     = op.threadVarName;
                    ti.startFuncName     = op.startFuncName;
                    ti.startFuncSymbolId = op.startFuncSymbolId;

                    m[ti.startFuncSymbolId].push_back(std::move(ti));
                }
            }
        }
    }

    return m;
}

// 当前 TU 里：函数 symbolId -> 在 model.functions 里的下标
using FuncIndex = QHash<QString, int>;

// 调用图：callerSymbolId -> { calleeSymbolId... }
using CallGraph = QHash<QString, QSet<QString>>;

// 递归遍历 AST，收集 calleeSymbolId
static void collectCallsFromAst(const QSharedPointer<AstNode> &root,
                                QSet<QString> &outCallees)
{
    if (!root) return;
    walkAst(root, [&outCallees](const AstNode &n) {
        if (!n.calleeSymbolId.isEmpty()) {
            outCallees.insert(n.calleeSymbolId);
        }
    });
}

// 建立 FuncIndex + CallGraph
static void buildFuncIndexAndCallGraph(const ProgramModel &model,
                                       FuncIndex &funcIndex,
                                       CallGraph &callGraph)
{
    funcIndex.clear();
    callGraph.clear();

    // 1) 函数索引
    for (int i = 0; i < model.functions.size(); ++i) {
        const FunctionInfo &f = model.functions[i];
        if (!f.symbolId.isEmpty())
            funcIndex.insert(f.symbolId, i);
    }

    // 2) 调用图
    for (const FunctionInfo &f : model.functions) {
        if (f.symbolId.isEmpty())
            continue;

        QSet<QString> &succ = callGraph[f.symbolId];

        for (const BlockInfo &b : f.blocks) {
            for (const StmtInfo &s : b.stmts) {
                collectCallsFromAst(s.tree, succ);
            }
        }
    }
}

// 任意函数 symbolId -> 能通过调用链到达这里的线程实例
using ThreadReachMap = QHash<QString, QVector<ThreadInstance>>;

static ThreadReachMap computeThreadReach(const ProgramModel &model,
                                         const ThreadMap    &threadsByEntry)
{
    FuncIndex funcIndex;
    CallGraph callGraph;
    buildFuncIndexAndCallGraph(model, funcIndex, callGraph);

    ThreadReachMap reach;

    // 对每个“入口函数 symbolId -> 线程列表”做一次 BFS/DFS
    for (auto it = threadsByEntry.constBegin();
         it != threadsByEntry.constEnd(); ++it)
    {
        const QString &entrySym = it.key();
        const QVector<ThreadInstance> &threads = it.value();

        if (threads.isEmpty())
            continue;

        // 从入口函数开始，沿着调用图往下走
        QSet<QString> visited;
        QVector<QString> worklist;
        worklist.push_back(entrySym);

        while (!worklist.isEmpty()) {
            const QString cur = worklist.back();
            worklist.pop_back();

            if (visited.contains(cur))
                continue;
            visited.insert(cur);

            // 只对当前 TU 里有定义的函数记录线程信息
            if (funcIndex.contains(cur)) {
                QVector<ThreadInstance> &vec = reach[cur];

                // 把 threads 里的信息合并进 vec，避免重复
                for (const ThreadInstance &ti : threads) {
                    bool exists = false;
                    for (const ThreadInstance &old : vec) {
                        if (old.threadVarName     == ti.threadVarName &&
                            old.startFuncSymbolId == ti.startFuncSymbolId)
                        {
                            exists = true;
                            break;
                        }
                    }
                    if (!exists)
                        vec.push_back(ti);
                }
            }

            // 继续沿调用图往下
            const QSet<QString> succ = callGraph.value(cur);
            for (const QString &callee : succ) {
                if (!visited.contains(callee))
                    worklist.push_back(callee);
            }
        }
    }

    return reach;
}


// 给一个 VarEvent 挂上线程信息（如果当前函数是某个线程入口）
static void attachThreadInfo(VarEvent &ev,
                             const FunctionInfo &func,
                             const ThreadReachMap &threadsByFunc)
{
    const auto it = threadsByFunc.constFind(func.symbolId);
    if (it == threadsByFunc.constEnd() || it->isEmpty())
        return;

    // 一个函数可能被多个线程调用，这里先简单记第 1 个，
    // 以后你可以改成 QVector<ThreadContext> 存多份。
    const ThreadInstance &th = it->first();
    ev.threadVarName   = th.threadVarName;
    ev.threadApi       = th.api;
    ev.threadEntryName = th.startFuncName;
}

// 使用 CFG（basic block + predecessors/successors）来标记 underLock / heldMutexes
static void markLockRegions(const ProgramModel &model,
                            QVector<VarEvent> &events)
{
    using LockSet = QSet<QString>; // 保存当前持有的 mutex 的 symbolId

    // 为了在块里按语句顺序处理，我们先按 (funcName, blockId) 聚合事件索引
    struct EvKey {
        QString funcName;
        int     blockId;
    };

    // 按函数逐个处理
    for (const FunctionInfo &func : model.functions) {

        // 1) 建 blockId -> BlockInfo* 映射，方便通过 id 找块
        QHash<int, const BlockInfo*> blocksById;
        for (const BlockInfo &b : func.blocks) {
            blocksById.insert(b.id, &b);
        }

        if (blocksById.isEmpty())
            continue;

        // 2) 为该函数收集属于它的事件，按 blockId 分组
        QHash<int, QVector<int>> eventsInBlock; // blockId -> indices in events

        for (int i = 0; i < events.size(); ++i) {
            const VarEvent &ev = events[i];
            if (ev.funcName != func.name)
                continue;
            if (!blocksById.contains(ev.blockId))
                continue;
            eventsInBlock[ev.blockId].push_back(i);
        }

        // 每个块内部，按行号排序事件（块内语句是线性的）
        for (auto it = eventsInBlock.begin(); it != eventsInBlock.end(); ++it) {
            QVector<int> &idxs = it.value();
            std::sort(idxs.begin(), idxs.end(), [&](int a, int b) {
                const VarEvent &ea = events[a];
                const VarEvent &eb = events[b];
                if (ea.line != eb.line)
                    return ea.line < eb.line;
                return a < b; // 稍微稳定一点
            });
        }

        // 3) 数据流状态：每个块的 in/out 锁集合
        QHash<int, LockSet> inLocks;
        QHash<int, LockSet> outLocks;

        // 初始化：都为空集（may-lock 分析：表示“目前还没发现任何路径持有锁”）
        for (auto it = blocksById.constBegin(); it != blocksById.constEnd(); ++it) {
            inLocks[it.key()]  = LockSet{};
            outLocks[it.key()] = LockSet{};
        }

        // 4) 迭代直到收敛（简单 worklist，这里直接 for 循环 + changed 标志）
        bool changed = true;
        int  iter    = 0;
        const int kMaxIter = 1000; // 防御性限制，避免异常 CFG 死循环

        while (changed && iter++ < kMaxIter) {
            changed = false;

            // 对函数里的所有块跑一遍
            for (const BlockInfo &b : func.blocks) {
                const int bid = b.id;

                // 4.1 计算 inLocks[bid] = 所有前驱 outLocks 的并集（may-lock）
                LockSet newIn;
                bool hasPred = false;
                for (int predId : b.predecessors) {
                    hasPred = true;
                    const LockSet &outP = outLocks[predId];
                    newIn.unite(outP);
                }

                // 没有前驱（入口块），inLocks 保持原值（一般是空集）
                if (!hasPred) {
                    newIn = inLocks[bid];
                }

                if (newIn != inLocks[bid]) {
                    inLocks[bid] = newIn;
                    changed = true;
                }

                // 4.2 从 inLocks[bid] 出发，按块内事件顺序模拟 lock/unlock
                LockSet cur = inLocks[bid];

                auto itEvBlock = eventsInBlock.constFind(bid);
                if (itEvBlock != eventsInBlock.constEnd()) {
                    const QVector<int> &evIdxs = itEvBlock.value();
                    for (int ei : evIdxs) {
                        VarEvent &ev = events[ei];

                        if (ev.action == QStringLiteral("MutexLock")) {
                            cur.insert(ev.symbolId);
                            continue;
                        }

                        if (ev.action == QStringLiteral("MutexUnlock")) {
                            cur.remove(ev.symbolId);
                            continue;
                        }

                        // 其它事件：记录当前持有的锁（may-lock：如果某条路径持有锁，就认为“可能在锁内”）
                        if (!cur.isEmpty()) {
                            ev.underLock = true;
                            QStringList held;
                            held.reserve(cur.size());
                            for (const QString &m : cur) {
                                held << m;
                            }
                            ev.heldMutexes = held;
                        }
                    }
                }

                // 4.3 更新 outLocks[bid]
                if (cur != outLocks[bid]) {
                    outLocks[bid] = cur;
                    changed = true;
                }
            }
        }
    }
}

// 使用 CFG 做一次“指针是否可能未初始化”的前向数据流分析，
// 在 PtrDeref 事件上标记 ev.isUninitPtrDeref = true.
static void markUninitializedPtrDerefs(const ProgramModel &model,
                                       QVector<VarEvent> &events)
{
    // symbolId -> 这一点上指针的 points-to / 未初始化 状态
    using PtrState = QHash<QString, PtrPointsTo>;

    // 按函数逐个分析（和 markLockRegions 的结构类似）
    for (const FunctionInfo &func : model.functions) {

        // 1) 收集本函数里的所有事件索引，按 blockId 分组
        QHash<int, QVector<int>> eventsInBlock; // blockId -> indices in events
        QSet<int> blockIds;

        for (int i = 0; i < events.size(); ++i) {
            const VarEvent &ev = events[i];
            if (ev.funcName != func.name)
                continue;
            if (ev.blockId < 0)
                continue;
            eventsInBlock[ev.blockId].push_back(i);
            blockIds.insert(ev.blockId);
        }

        if (blockIds.isEmpty())
            continue;

        // 2) 确定“哪些 symbolId 是指针变量”，以及它们的作用域
        struct PtrMeta {
            bool isPtr    = false;
            bool isGlobal = false;
            bool isLocal  = false;
            bool isParam  = false;
        };

        QHash<QString, PtrMeta> ptrMeta; // symbolId -> meta

        for (int i = 0; i < events.size(); ++i) {
            const VarEvent &ev = events[i];
            if (ev.funcName != func.name)
                continue;
            if (ev.symbolId.isEmpty())
                continue;
            if (!isPointerType(ev.varType))
                continue;

            PtrMeta &m = ptrMeta[ev.symbolId];
            m.isPtr = true;
            m.isGlobal = m.isGlobal || ev.isGlobal;
            m.isLocal  = m.isLocal  || ev.isLocal;
            m.isParam  = m.isParam  || ev.isParam;
        }

        if (ptrMeta.isEmpty())
            continue; // 这个函数里没有指针相关事件，跳过

        // 3) baseState：函数入口处的默认“可能未初始化”状态
        //    - 局部指针：默认 mayBeUninit = true
        //    - 参数/全局指针：默认 false（假设已初始化）
        PtrState baseState;
        for (auto it = ptrMeta.constBegin(); it != ptrMeta.constEnd(); ++it) {
            const QString &sym = it.key();
            const PtrMeta &m   = it.value();

            PtrPointsTo pi;
            if (m.isLocal && !m.isGlobal && !m.isParam) {
                // 局部指针：入口时一定未初始化
                pi.mayBeUninit = true;
                pi.mayBeInit   = false;
            } else {
                // 参数 / 全局：默认认为已经初始化过
                pi.mayBeUninit = false;
                pi.mayBeInit   = true;
            }
            baseState.insert(sym, pi);
        }

        // 4) 构建 blockId -> BlockInfo* 映射（方便找 predecessors）
        QHash<int, const BlockInfo*> blocksById;
        for (const BlockInfo &b : func.blocks) {
            blocksById.insert(b.id, &b);
        }

        // 5) in/out 状态：每个块入口与出口的指针未初始化状态
        QHash<int, PtrState> inState;
        QHash<int, PtrState> outState;

        for (int bid : blockIds) {
            inState[bid]  = baseState;
            outState[bid] = baseState;
        }

        // 6) 数据流迭代，直到稳定
        bool changed   = true;
        int  iter      = 0;
        const int kMaxIter = 1000;

        while (changed && iter++ < kMaxIter) {
            changed = false;

            for (const BlockInfo &b : func.blocks) {
                const int bid = b.id;
                if (!blockIds.contains(bid))
                    continue; // 这个块里没有事件，先忽略（也可以照样处理）

                // 6.1 计算 inState[bid]：来自所有前驱 outState 的 OR（may-uninit）
                PtrState newIn;          // 一开始是空
                bool hasPred = false;

                for (int predId : b.predecessors) {
                    if (!blockIds.contains(predId))
                        continue;
                    hasPred = true;

                    const PtrState &ps = outState[predId];

                    for (auto itMeta = ptrMeta.constBegin();
                         itMeta != ptrMeta.constEnd(); ++itMeta)
                    {
                        const QString &sym = itMeta.key();

                        const PtrPointsTo basePi = baseState.value(sym);
                        const PtrPointsTo predPi = ps.value(sym, basePi);

                        // 第一次看到这个 sym，就直接用这个前驱的状态当作初始值
                        PtrPointsTo curPi = newIn.contains(sym) ? newIn.value(sym)
                                                                : predPi;

                        // may-uninit：任一前驱为 true -> true
                        if (predPi.mayBeUninit)
                            curPi.mayBeUninit = true;

                        // may-init：任一前驱为 true -> true（前提是你已经在初始化事件里设过）
                        if (predPi.mayBeInit)
                            curPi.mayBeInit = true;

                        newIn.insert(sym, curPi);
                    }
                }

                // 没有前驱：函数入口块，inState 就是 baseState
                if (!hasPred) {
                    newIn = baseState;
                }

                if (newIn != inState[bid]) {
                    inState[bid] = newIn;
                    changed = true;
                }

                // 6.2 从 inState[bid] 出发，按块内事件顺序更新 outState[bid]
                PtrState curState = inState[bid];

                auto itEvBlock = eventsInBlock.constFind(bid);
                if (itEvBlock != eventsInBlock.constEnd()) {
                    QVector<int> evIdxs = itEvBlock.value();
                    // 块内按行号排序（原逻辑不变）
                    std::sort(evIdxs.begin(), evIdxs.end(),
                              [&](int a, int b) {
                                  const VarEvent &ea = events[a];
                                  const VarEvent &eb = events[b];
                                  if (ea.line != eb.line)
                                      return ea.line < eb.line;
                                  return a < b;
                              });

                    for (int ei : evIdxs) {
                        VarEvent &ev = events[ei];

                        if (ev.symbolId.isEmpty())
                            continue;
                        if (!isPointerType(ev.varType))
                            continue;
                        if (!ptrMeta.contains(ev.symbolId))
                            continue;

                        PtrPointsTo basePi = baseState.value(ev.symbolId);
                        PtrPointsTo curPi  = curState.value(ev.symbolId, basePi);

                        bool mayBeUninit = curPi.mayBeUninit;
                        bool mayBeInit   = curPi.mayBeInit;

                        // 1) 指针解引用：根据两位状态来区分
                        if (ev.action == QStringLiteral("PtrDeref")) {
                            if (mayBeUninit) {
                                // 兼容原来的字段：只要有风险就为 true
                                ev.isUninitPtrDeref = true;

                                // 新增一个字段，用来区分“一定 / 可能”
                                // 你可以在 VarEvent 里加：
                                //   bool isDefiniteUninitPtrDeref = false;
                                ev.isDefiniteUninitPtrDeref = !mayBeInit;
                                //   - mayBeUninit == true && mayBeInit == false → 一定未初始化
                                //   - mayBeUninit == true && mayBeInit == true  → 可能未初始化
                            }
                        }

                        // 2) 指针初始化/分配：从这之后沿当前路径上肯定已初始化
                        if (ev.action.startsWith(QStringLiteral("PtrInit")) ||
                            ev.action.startsWith(QStringLiteral("PtrAlloc")))
                        {
                            mayBeUninit = false;
                            mayBeInit   = true;
                        }

                        curPi.mayBeUninit = mayBeUninit;
                        curPi.mayBeInit   = mayBeInit;
                        curState.insert(ev.symbolId, curPi);
                    }
                }

                if (curState != outState[bid]) {
                    outState[bid] = curState;
                    changed = true;
                }
            }
        }
    }
}

} // anonymous namespace

// ---------- 主分析逻辑：生成 VarEvent + 汇总 ----------

AnalysisOutput analyzeProgram(const ProgramModel &model)
{
    AnalysisOutput out;

    // 0) 先把“线程入口函数”信息收集起来
    ThreadMap threadsByEntry = buildThreadMap(model);

    // 再沿调用图传播：任意函数 -> 能在其中跑的线程
    ThreadReachMap threadsByFunc = computeThreadReach(model, threadsByEntry);

    // 1) 先生成所有细粒度事件（Use / Assign / Call / 指针相关）
    for (const auto &func : model.functions) {

        AnalysisState state; // 目前简单版：只是占位，将来可以扩展为数据流

        for (const auto &block : func.blocks) {
            for (const auto &stmt : block.stmts) {

                if (stmt.tree) {
                    ExprSummary sum = summarizeStmtTree(stmt.tree);

                    analysis::VarEvent sev;
                    sev.funcName = func.name;
                    sev.blockId = block.id;
                    sev.line = stmt.line;
                    sev.code = stmt.code;
                    sev.action = "StmtSummary";

                    QStringList parts;
                    if (sum.hasPointerAssign) parts << "指针赋值/初始化";
                    if (sum.hasCondOp)        parts << "条件运算 ?:";
                    if (sum.hasMalloc)        parts << "malloc/calloc/realloc";
                    if (sum.hasNew)           parts << "new";
                    if (sum.hasDeref)         parts << "解引用";
                    if (sum.hasAddrOf)        parts << "取地址 &";
                    if (sum.hasCall)          parts << "函数调用";
                    if (sum.hasBinaryOp)      parts << "二元运算";
                    if (sum.hasMemberAccess)  parts << "成员访问";

                    if (!sum.usedVars.isEmpty()) {
                        parts << QString("使用变量%1个").arg(sum.usedVars.size());
                    }

                    if (!sum.calledFuncs.isEmpty()) {
                        parts << QString("调用函数%1个").arg(sum.calledFuncs.size());
                    }

                    sev.detail = parts.join("；");
                    out.events.push_back(sev);
                }


                if (!stmt.tree)
                    continue;

                const QString stmtCode = stmt.code;
                const int     stmtLine = stmt.line;
                const int     blockId  = block.id;

                walkAst(stmt.tree, [&](const AstNode &n) {
                    // -------- 1) 普通变量使用：DeclRefExpr --------
                    if (n.kind == "DeclRefExpr" && !n.symbolId.isEmpty()) {
                        auto &vs = state.vars[n.symbolId];
                        vs.everRead = true;

                        VarEvent ev;
                        ev.funcName = func.name;
                        ev.blockId  = blockId;
                        ev.line     = stmtLine;
                        attachThreadInfo(ev, func, threadsByFunc);
                        ev.varName  = n.varName;
                        ev.symbolId = n.symbolId;
                        ev.varType  = n.varType;
                        ev.isGlobal = n.isGlobal;
                        ev.isLocal  = n.isLocal;
                        ev.isParam  = n.isParam;
                        ev.action   = QStringLiteral("Use");
                        ev.detail   = QStringLiteral("变量被读取");
                        ev.code     = stmtCode;
                        out.events.push_back(std::move(ev));
                    }

                    // -------- 2) 指针解引用：*p / p[i] / delete p --------
                    // 2.1 显式 *p
                    if (n.kind == "UnaryOperator" && n.op == "*") {
                        if (!n.children.isEmpty()) {
                            const AstNode *ptr =
                                findFirstPtrRef(stripCastsAndParens(n.children.first().data()));
                            if (ptr) {
                                auto &vs = state.vars[ptr->symbolId];
                                vs.everRead = true;

                                VarEvent ev;
                                ev.funcName = func.name;
                                ev.blockId  = blockId;
                                ev.line     = stmtLine;
                                attachThreadInfo(ev, func, threadsByFunc);
                                ev.varName  = ptr->varName;
                                ev.symbolId = ptr->symbolId;
                                ev.varType  = ptr->varType;
                                ev.isGlobal = ptr->isGlobal;
                                ev.isLocal  = ptr->isLocal;
                                ev.isParam  = ptr->isParam;
                                ev.action   = QStringLiteral("PtrDeref");
                                ev.detail   = QStringLiteral("指针 %1 通过 * 解引用").arg(ptr->varName);
                                ev.code     = stmtCode;
                                out.events.push_back(std::move(ev));
                            }
                        }
                    }

                    // 2.2 数组下标：p[i] 也视为一次解引用
                    if (n.kind == "ArraySubscriptExpr") {
                        if (!n.children.isEmpty()) {
                            const AstNode *base =
                                stripCastsAndParens(n.children.first().data());
                            const AstNode *ptr = findFirstPtrRef(base);
                            if (ptr) {
                                auto &vs = state.vars[ptr->symbolId];
                                vs.everRead = true;

                                VarEvent ev;
                                ev.funcName = func.name;
                                ev.blockId  = blockId;
                                ev.line     = stmtLine;
                                attachThreadInfo(ev, func, threadsByFunc);
                                ev.varName  = ptr->varName;
                                ev.symbolId = ptr->symbolId;
                                ev.varType  = ptr->varType;
                                ev.isGlobal = ptr->isGlobal;
                                ev.isLocal  = ptr->isLocal;
                                ev.isParam  = ptr->isParam;
                                ev.action   = QStringLiteral("PtrDeref");
                                ev.detail   = QStringLiteral("指针 %1 通过 [] 解引用").arg(ptr->varName);
                                ev.code     = stmtCode;
                                out.events.push_back(std::move(ev));
                            }
                        }
                    }

                    // -------- 3) 指针赋值 / 初始化 --------
                    if (n.kind == "InitAssign" ||
                        (n.kind == "BinaryOperator" && n.op == "="))
                    {
                        if (n.children.size() >= 2) {
                            const AstNode &lhsNode = *n.children[0];
                            const AstNode &rhsNode = *n.children[1];

                            const AstNode *ptrLhs = findPointerLHS(lhsNode);
                            if (ptrLhs && !ptrLhs->symbolId.isEmpty()) {
                                auto &vs = state.vars[ptrLhs->symbolId];
                                vs.everAssigned = true;

                                QString srcName;
                                QString srcSym;
                                PtrAssignKind pk =
                                    classifyPointerRHS(rhsNode, srcName, srcSym);

                                VarEvent ev;
                                ev.funcName = func.name;
                                ev.blockId  = blockId;
                                ev.line     = stmtLine;
                                attachThreadInfo(ev, func, threadsByFunc);
                                ev.varName  = ptrLhs->varName;
                                ev.symbolId = ptrLhs->symbolId;
                                ev.varType  = ptrLhs->varType;
                                ev.isGlobal = ptrLhs->isGlobal;
                                ev.isLocal  = ptrLhs->isLocal;
                                ev.isParam  = ptrLhs->isParam;
                                ev.code     = stmtCode;

                                auto &ptrInfo = state.ptrPoints[ptrLhs->symbolId];
                                ptrInfo.hasInfo = true;          // 从这一刻开始有信息了
                                ptrInfo.vars.clear();
                                ptrInfo.mayNull = false;
                                ptrInfo.mayHeap = false;

                                switch (pk) {
                                case PtrAssignKind::AddrOf:
                                    ev.action = QStringLiteral("PtrInitAddrOf");
                                    ev.detail = QStringLiteral("指针 %1 = &%2")
                                                    .arg(ptrLhs->varName,
                                                         srcName);
                                    vs.lastValueHint =
                                        QStringLiteral("addrOf %1").arg(srcName);

                                    // points-to：指向某个变量
                                    ptrInfo.vars.insert(srcSym);  // srcSym 是 &谁 的 symbolId
                                    ptrInfo.mayHeap = false;
                                    ptrInfo.mayNull = false;
                                    break;

                                    break;
                                case PtrAssignKind::Null:
                                    ev.action = QStringLiteral("PtrInitNull");
                                    ev.detail = QStringLiteral("指针 %1 赋值为 nullptr")
                                                    .arg(ptrLhs->varName);
                                    vs.lastValueHint = QStringLiteral("null");

                                    // points-to：只可能是 null
                                    ptrInfo.vars.clear();
                                    ptrInfo.mayNull = true;
                                    ptrInfo.mayHeap = false;

                                    break;
                                case PtrAssignKind::Copy:
                                    ev.action = QStringLiteral("PtrAlias");
                                    ev.detail = QStringLiteral("指针 %1 = %2 (别名)")
                                                    .arg(ptrLhs->varName,
                                                         srcName);
                                    vs.lastValueHint =
                                        QStringLiteral("alias of %1").arg(srcName);

                                    //更新 points-to：拷贝源指针的指向信息
                                    ptrInfo = state.ptrPoints.value(srcSym, PtrPointsTo{});
                                    ptrInfo.hasInfo = true;

                                    break;
                                case PtrAssignKind::FromMalloc:
                                    ev.action = QStringLiteral("PtrAllocMalloc");
                                    ev.detail = QStringLiteral("指针 %1 从 malloc/calloc/realloc 分配")
                                                    .arg(ptrLhs->varName);
                                    vs.lastValueHint = QStringLiteral("from malloc");

                                    // points-to：指向堆
                                    ptrInfo.vars.clear();
                                    ptrInfo.mayHeap = true;
                                    ptrInfo.mayNull = false;

                                    break;
                                case PtrAssignKind::FromNew:
                                    ev.action = QStringLiteral("PtrAllocNew");
                                    ev.detail = QStringLiteral("指针 %1 从 new 表达式分配")
                                                    .arg(ptrLhs->varName);
                                    vs.lastValueHint = QStringLiteral("from new");

                                    // 更新 points-to：指向堆（C++ new）
                                    ptrInfo.vars.clear();
                                    ptrInfo.mayHeap = true;
                                    ptrInfo.mayNull = false;

                                    break;
                                case PtrAssignKind::Conditional:
                                    ev.action = QStringLiteral("PtrInitConditional");
                                    ev.detail = QStringLiteral("指针 %1 通过条件表达式初始化")
                                                    .arg(ptrLhs->varName);
                                    vs.lastValueHint = QStringLiteral("conditional init");

                                    ptrInfo.hasInfo = true;
                                    ptrInfo.vars.clear();
                                    ptrInfo.mayHeap = true;
                                    ptrInfo.mayNull = true;
                                    break;
                                case PtrAssignKind::Unknown:
                                default:
                                    ev.action = QStringLiteral("PtrInit");
                                    ev.detail = QStringLiteral("指针 %1 赋值/初始化")
                                                    .arg(ptrLhs->varName);
                                    vs.lastValueHint = QStringLiteral("unknown init");

                                    // 更新 points-to：我们知道被重新赋值了，但不知道具体指向，设成“未知”
                                    ptrInfo.vars.clear();
                                    ptrInfo.mayNull = true;   // 保守一点：可能为 null
                                    ptrInfo.mayHeap = true;   // 也可能指向堆

                                    break;
                                }

                                out.events.push_back(std::move(ev));
                            }
                        }
                    }

                    // 同一个节点上，我们仍然保留“通用 Assign” 事件，方便非指针变量统计
                    // 通用赋值事件：只处理真正的赋值运算符
                    if (n.kind == "BinaryOperator" ||
                        n.kind == "CompoundAssignOperator")
                    {
                        // 过滤掉 `<`, `&`, `%` 这些普通运算
                        if (!isAssignmentOp(n.op))
                            return;

                        if (n.children.isEmpty())
                            return;

                        const auto &lhsExpr = n.children.first();
                        const AstNode *lhsVar = findFirstDeclRef(lhsExpr.data());
                        if (!lhsVar || lhsVar->symbolId.isEmpty())
                            return;

                        auto &vs = state.vars[lhsVar->symbolId];
                        vs.everAssigned = true;
                        vs.lastValueHint = QStringLiteral("op=%1").arg(n.op);

                        VarEvent ev;
                        ev.funcName = func.name;
                        ev.blockId  = blockId;
                        ev.line     = stmtLine;
                        attachThreadInfo(ev, func, threadsByFunc);
                        ev.varName  = lhsVar->varName;
                        ev.symbolId = lhsVar->symbolId;
                        ev.varType  = n.varType;
                        ev.isGlobal = n.isGlobal;
                        ev.isLocal  = n.isLocal;
                        ev.isParam  = n.isParam;
                        ev.action   = QStringLiteral("Assign");
                        ev.detail   = QStringLiteral("通过操作符 `%1` 赋值").arg(n.op);
                        ev.code     = stmtCode;
                        out.events.push_back(std::move(ev));
                    }

                    // -------- 4) malloc/free/new/delete/lock/unlock 等调用 --------
                    if (n.kind == "CallExpr" || n.kind == "CXXMemberCallExpr") {

                        // 成员函数调用时 calleeName 为空，用 memberName 顶上
                        QString calleeName = n.calleeName;
                        if (calleeName.isEmpty())
                            calleeName = n.memberName;

                        // 4.1 通用调用事件
                        if (!calleeName.isEmpty()) {
                            VarEvent ev;
                            ev.funcName = func.name;
                            ev.blockId  = blockId;
                            ev.line     = stmtLine;
                            attachThreadInfo(ev, func, threadsByFunc);
                            ev.varName  = QString(); // 不对应某个具体变量
                            ev.symbolId = n.calleeSymbolId;
                            ev.varType  = n.varType;
                            ev.isGlobal = n.isGlobal;
                            ev.isLocal  = n.isLocal;
                            ev.isParam  = n.isParam;
                            ev.action   = QStringLiteral("Call");
                            ev.detail   = QStringLiteral("调用函数 %1()").arg(calleeName);
                            ev.code     = stmtCode;
                            out.events.push_back(std::move(ev));
                        }

                        // 4.2 特判 free(p)
                        if (calleeName == QLatin1String("free")) {
                            const AstNode *argPtr = nullptr;
                            for (const auto &child : n.children) {
                                argPtr = findFirstPtrRefExcluding(child.data(), n.calleeSymbolId);
                                if (argPtr) break;
                            }

                            if (argPtr && !argPtr->symbolId.isEmpty()) {
                                auto &vs = state.vars[argPtr->symbolId];
                                vs.everFreed = true;

                                VarEvent ev;
                                ev.funcName = func.name;
                                ev.blockId  = blockId;
                                ev.line     = stmtLine;
                                attachThreadInfo(ev, func, threadsByFunc);
                                ev.varName  = argPtr->varName;
                                ev.symbolId = argPtr->symbolId;
                                ev.varType  = argPtr->varType;
                                ev.isGlobal = argPtr->isGlobal;
                                ev.isLocal  = argPtr->isLocal;
                                ev.isParam  = argPtr->isParam;
                                ev.action   = QStringLiteral("PtrFree");
                                ev.detail   = QStringLiteral("指针 %1 被 free")
                                                .arg(argPtr->varName);
                                ev.code     = stmtCode;
                                out.events.push_back(std::move(ev));
                            }
                        }

                        // 4.3 特判：mutex.lock() / mutex.unlock()
                        if (calleeName == QLatin1String("lock") ||
                            calleeName == QLatin1String("unlock"))
                        {
                            // 在参数/对象子树里找第一个 mutex 变量引用
                            const AstNode *mutexVar = nullptr;
                            for (const auto &child : n.children) {
                                const AstNode *cand = findFirstDeclRef(child.data());
                                if (cand && !cand->symbolId.isEmpty()) {
                                    // 如果想更严格，可以在这里加:
                                    // if (!isSyncType(cand->varType)) continue;
                                    mutexVar = cand;
                                    break;
                                }
                            }

                            if (mutexVar) {
                                auto &vs = state.vars[mutexVar->symbolId];
                                vs.everRead = true;

                                VarEvent ev;
                                ev.funcName = func.name;
                                ev.blockId  = blockId;
                                ev.line     = stmtLine;
                                attachThreadInfo(ev, func, threadsByFunc);
                                ev.varName  = mutexVar->varName;
                                ev.symbolId = mutexVar->symbolId;
                                ev.varType  = mutexVar->varType;
                                ev.isGlobal = mutexVar->isGlobal;
                                ev.isLocal  = mutexVar->isLocal;
                                ev.isParam  = mutexVar->isParam;
                                ev.code     = stmtCode;

                                if (calleeName == QLatin1String("lock")) {
                                    ev.action = QStringLiteral("MutexLock");
                                    ev.detail = QStringLiteral("对互斥量 %1 调用 lock()")
                                                    .arg(mutexVar->varName);
                                } else {
                                    ev.action = QStringLiteral("MutexUnlock");
                                    ev.detail = QStringLiteral("对互斥量 %1 调用 unlock()")
                                                    .arg(mutexVar->varName);
                                }

                                out.events.push_back(std::move(ev));
                            }
                        }
                    }

                    // C++ delete / delete[]
                    if (n.kind == "CXXDeleteExpr") {
                        if (!n.children.isEmpty()) {
                            const AstNode *ptr =
                                findFirstPtrRef(
                                    stripCastsAndParens(
                                        n.children.first().data()));
                            if (ptr && !ptr->symbolId.isEmpty()) {
                                auto &vs = state.vars[ptr->symbolId];
                                vs.everFreed = true;

                                VarEvent ev;
                                ev.funcName = func.name;
                                ev.blockId  = blockId;
                                ev.line     = stmtLine;

                                attachThreadInfo(ev, func, threadsByFunc);
                                ev.varName  = ptr->varName;
                                ev.symbolId = ptr->symbolId;
                                ev.varType  = ptr->varType;
                                ev.isGlobal = ptr->isGlobal;
                                ev.isLocal  = ptr->isLocal;
                                ev.isParam  = ptr->isParam;
                                ev.action   = QStringLiteral("PtrDelete");
                                ev.detail   = QStringLiteral("指针 %1 被 delete/delete[]")
                                                .arg(ptr->varName);
                                ev.code     = stmtCode;
                                out.events.push_back(std::move(ev));
                            }
                        }
                    }

                });
            }
        }
    }

    // 1.5) 根据 MutexLock/MutexUnlock 标记 underLock / heldMutexes
    markLockRegions(model, out.events);

    // 1.6) 基于 CFG 分析哪些指针解引用“可能未初始化”
    markUninitializedPtrDerefs(model, out.events);

    // 2) 基于事件按变量做汇总（只写不读 / 只读不写 等）
    QSet<QString> varIds;
    QHash<QString, bool> hasAssign;
    QHash<QString, bool> hasUse;
    QHash<QString, VarEvent> firstAssign;
    QHash<QString, VarEvent> firstUse;

    for (const auto &ev : out.events) {
        if (ev.symbolId.isEmpty())
            continue;

        varIds.insert(ev.symbolId);

        if (ev.action == QStringLiteral("Assign")) {
            hasAssign[ev.symbolId] = true;
            if (!firstAssign.contains(ev.symbolId)) {
                firstAssign.insert(ev.symbolId, ev);
            }
        } else if (ev.action == QStringLiteral("Use")) {
            hasUse[ev.symbolId] = true;
            if (!firstUse.contains(ev.symbolId)) {
                firstUse.insert(ev.symbolId, ev);
            }
        }
    }

    // 3) 填充 AnalysisOutput::vars
    for (const QString &varId : varIds) {
        VarSummary sum;
        sum.symbolId    = varId;
        sum.everAssigned = hasAssign.value(varId, false);
        sum.everUsed     = hasUse.value(varId, false);

        // 从 ProgramModel 里找 VarInfo（拿名字、作用域）
        auto itVarInfo = model.varsBySymbolId.constFind(varId);
        if (itVarInfo != model.varsBySymbolId.constEnd()) {
            const VarInfo &info = itVarInfo.value();
            sum.name = info.name;

            if (info.isGlobal)      sum.scopeDesc = QStringLiteral("全局变量");
            else if (info.isParam)  sum.scopeDesc = QStringLiteral("参数");
            else if (info.isLocal)  sum.scopeDesc = QStringLiteral("局部变量");
            else                    sum.scopeDesc = QStringLiteral("未知作用域");
        } else {
            // 退回到事件里的名字
            if (firstAssign.contains(varId) &&
                !firstAssign[varId].varName.isEmpty())
            {
                sum.name = firstAssign[varId].varName;
            } else if (firstUse.contains(varId) &&
                       !firstUse[varId].varName.isEmpty())
            {
                sum.name = firstUse[varId].varName;
            } else {
                sum.name = QStringLiteral("<unknown>");
            }
            sum.scopeDesc = QStringLiteral("未知作用域");
        }

        if (firstAssign.contains(varId)) {
            sum.firstAssign    = firstAssign[varId];
            sum.hasFirstAssign = true;
        }
        if (firstUse.contains(varId)) {
            sum.firstUse    = firstUse[varId];
            sum.hasFirstUse = true;
        }

        out.vars.insert(varId, sum);
    }

    return out;
}

} // namespace analysis
