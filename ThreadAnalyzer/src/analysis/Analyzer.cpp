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

// 粗略判断一个类型是不是 mutex，后面你可以根据实际类型字符串再细化
static bool isMutexType(const QString &t)
{
    // 例如 "std::mutex" / "std::recursive_mutex" 等
    // 这里先只看有没有 "mutex" 子串
    return t.contains("mutex");
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
    FromNew      // p = new T(...)
};

// 对 RHS 表达式做分类
static PtrAssignKind classifyPointerRHS(const AstNode &rhsRoot,
                                        QString &srcName,
                                        QString &srcSymbolId)
{
    const AstNode *rhs = stripCastsAndParens(&rhsRoot);
    if (!rhs) return PtrAssignKind::Unknown;

    // 1) 取地址：p = &x;
    if (rhs->kind == "UnaryOperator" && rhs->op == "&") {
        if (!rhs->children.isEmpty()) {
            const AstNode *target = stripCastsAndParens(rhs->children.first().data());
            if (target && target->kind == "DeclRefExpr") {
                srcName     = target->varName;
                srcSymbolId = target->symbolId;
            }
        }
        return PtrAssignKind::AddrOf;
    }

    // 2) nullptr：p = nullptr;
    if (rhs->kind == "CXXNullPtrLiteralExpr") {
        return PtrAssignKind::Null;
    }

    // 3) 0 常量：p = 0;  (IntegerLiteral + 类型是指针)
    if (rhs->kind == "IntegerLiteral" && isPointerType(rhs->varType)) {
        return PtrAssignKind::Null;
    }

    // 4) 直接拷贝：p = q; （DeclRefExpr，其类型是指针）
    if (rhs->kind == "DeclRefExpr" && isPointerType(rhs->varType)) {
        srcName     = rhs->varName;
        srcSymbolId = rhs->symbolId;
        return PtrAssignKind::Copy;
    }

    // 5) malloc / calloc / realloc
    if (rhs->kind == "CallExpr") {
        if (rhs->calleeName == "malloc" ||
            rhs->calleeName == "calloc" ||
            rhs->calleeName == "realloc")
        {
            return PtrAssignKind::FromMalloc;
        }
    }

    // 6) new 表达式
    if (rhs->kind == "CXXNewExpr") {
        return PtrAssignKind::FromNew;
    }

    // 7) 其它复杂情况暂时算 Unknown，后面你可以继续细化
    return PtrAssignKind::Unknown;
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
    using PtrState = QHash<QString, bool>; // symbolId -> mayBeUninit

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
            bool mayBeUninit = false;
            if (m.isLocal && !m.isGlobal && !m.isParam)
                mayBeUninit = true;
            baseState.insert(sym, mayBeUninit);
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
                PtrState newIn = baseState;
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
                        bool baseVal = baseState.value(sym, false);
                        bool curVal  = newIn.value(sym, baseVal);
                        bool predVal = ps.value(sym, baseVal);
                        // may-uninit：只要某个前驱是 true，就保持 true
                        if (predVal)
                            curVal = true;
                        newIn.insert(sym, curVal);
                    }
                }

                // 没有前驱（入口块），inState 就是 baseState
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
                    // 块内按行号排序
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

                        bool baseVal = baseState.value(ev.symbolId, false);
                        bool mayBeUninit = curState.value(ev.symbolId, baseVal);

                        // 指针解引用：如果当前状态“可能未初始化”，标记这次解引用
                        if (ev.action == QStringLiteral("PtrDeref")) {
                            if (mayBeUninit) {
                                ev.isUninitPtrDeref = true;
                            }
                            // 解引用本身不改变初始化状态
                        }

                        // 指针初始化/分配：认为从这之后“肯定已经初始化过”
                        if (ev.action.startsWith(QStringLiteral("PtrInit")) ||
                            ev.action.startsWith(QStringLiteral("PtrAlloc")))
                        {
                            mayBeUninit = false;
                        }

                        curState.insert(ev.symbolId, mayBeUninit);
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
                                ev.varType  = n.varType;
                                ev.isGlobal = n.isGlobal;
                                ev.isLocal  = n.isLocal;
                                ev.isParam  = n.isParam;
                                ev.code     = stmtCode;

                                switch (pk) {
                                case PtrAssignKind::AddrOf:
                                    ev.action = QStringLiteral("PtrInitAddrOf");
                                    ev.detail = QStringLiteral("指针 %1 = &%2")
                                                    .arg(ptrLhs->varName,
                                                         srcName);
                                    vs.lastValueHint =
                                        QStringLiteral("addrOf %1").arg(srcName);
                                    break;
                                case PtrAssignKind::Null:
                                    ev.action = QStringLiteral("PtrInitNull");
                                    ev.detail = QStringLiteral("指针 %1 赋值为 nullptr/0")
                                                    .arg(ptrLhs->varName);
                                    vs.lastValueHint = QStringLiteral("null");
                                    break;
                                case PtrAssignKind::Copy:
                                    ev.action = QStringLiteral("PtrAlias");
                                    ev.detail = QStringLiteral("指针 %1 = %2 (别名)")
                                                    .arg(ptrLhs->varName,
                                                         srcName);
                                    vs.lastValueHint =
                                        QStringLiteral("alias of %1").arg(srcName);
                                    break;
                                case PtrAssignKind::FromMalloc:
                                    ev.action = QStringLiteral("PtrAllocMalloc");
                                    ev.detail = QStringLiteral("指针 %1 从 malloc/calloc/realloc 分配")
                                                    .arg(ptrLhs->varName);
                                    vs.lastValueHint = QStringLiteral("from malloc");
                                    break;
                                case PtrAssignKind::FromNew:
                                    ev.action = QStringLiteral("PtrAllocNew");
                                    ev.detail = QStringLiteral("指针 %1 从 new 表达式分配")
                                                    .arg(ptrLhs->varName);
                                    vs.lastValueHint = QStringLiteral("from new");
                                    break;
                                case PtrAssignKind::Unknown:
                                default:
                                    ev.action = QStringLiteral("PtrInit");
                                    ev.detail = QStringLiteral("指针 %1 赋值/初始化")
                                                    .arg(ptrLhs->varName);
                                    vs.lastValueHint = QStringLiteral("unknown init");
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
