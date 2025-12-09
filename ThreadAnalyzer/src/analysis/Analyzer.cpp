// analysis/Analyzer.cpp
#include "analysis/Analyzer.h"
#include <QStack>
#include <QSet>
#include <functional>
#include <QDebug>

namespace analysis {

// ---------- 内部小工具：遍历 AST / 指针分类 ----------

namespace {
// 如果为 true：参数/全局指针在函数入口默认视为“已初始化”
// 如果为 false：参数/全局指针在函数入口视为“未知”（既可能已初始化，也可能未初始化）
static constexpr bool kAssumeParamAndGlobalInitialized = true;

// === Tarjan 强连通分量（SCC） ===

static void tarjanDFS(
    const QString &u,
    const QHash<QString, QSet<QString>> &graph,
    QHash<QString, int> &dfn,
    QHash<QString, int> &low,
    QStack<QString> &stk,
    QSet<QString> &inStack,
    int &time,
    QVector<QVector<QString>> &sccList)
{
    dfn[u] = low[u] = ++time;
    stk.push(u);
    inStack.insert(u);

    for (const QString &v : graph.value(u)) {
        if (!dfn.contains(v)) {
            tarjanDFS(v, graph, dfn, low, stk, inStack, time, sccList);
            low[u] = qMin(low[u], low[v]);
        } else if (inStack.contains(v)) {
            low[u] = qMin(low[u], dfn[v]);
        }
    }

    // 检测到一个 SCC 根
    if (low[u] == dfn[u]) {
        QVector<QString> scc;
        while (true) {
            QString x = stk.pop();
            inStack.remove(x);
            scc.push_back(x);
            if (x == u) break;
        }
        sccList.push_back(scc);
    }
}

    // === 基于 Tarjan 的 SCC + 拓扑排序 ===

static QVector<QVector<QString>> computeSccTopoOrder(
    const QHash<QString, QSet<QString>> &graph)
{
    QHash<QString, int> dfn, low;
    QStack<QString> stk;
    QSet<QString> inStack;
    QVector<QVector<QString>> sccList;

    int time = 0;
    for (auto it = graph.constBegin(); it != graph.constEnd(); ++it) {
        const QString &u = it.key();
        if (!dfn.contains(u)) {
            tarjanDFS(u, graph, dfn, low, stk, inStack, time, sccList);
        }
    }

    // 若每个 SCC 是一个节点，则构造一个 DAG
    QHash<QString, int> belong;
    for (int i = 0; i < sccList.size(); ++i) {
        for (const QString &f : sccList[i]) {
            belong[f] = i;
        }
    }

    // 构建 SCC DAG 边（有向无环图）
    QVector<QSet<int>> dag(sccList.size());
    QVector<int> indeg(sccList.size(), 0);

    for (auto it = graph.constBegin(); it != graph.constEnd(); ++it) {
        const QString &u = it.key();
        for (const QString &v : it.value()) {
            int a = belong[u];
            int b = belong[v];
            if (a != b && !dag[a].contains(b)) {
                dag[a].insert(b);
                indeg[b]++;
            }
        }
    }

    // Kahn 拓扑排序 SCC DAG
    QVector<QVector<QString>> topoGroups;
    QVector<int> q;
    for (int i = 0; i < indeg.size(); ++i) {
        if (indeg[i] == 0)
            q.push_back(i);
    }

    while (!q.empty()) {
        int u = q.back();
        q.pop_back();

        topoGroups.push_back(sccList[u]);  // 这是一个 SCC 组

        for (int v : dag[u]) {
            indeg[v]--;
            if (indeg[v] == 0)
                q.push_back(v);
        }
    }

    qDebug().noquote() << "===== CallGraph SCC topo (caller -> callee, root -> leaf) =====";
    for (int gi = 0; gi < topoGroups.size(); ++gi) {
        const auto &group = topoGroups[gi];
        QStringList syms;
        for (const QString &sym : group)
            syms << sym;
        qDebug().noquote()
            << "  SCC group" << gi
            << ":" << syms.join(QStringLiteral(", "));
    }

    return topoGroups;  // 每个 group 是一个 SCC，顺序已拓扑化
}

// 简单判断一个类型串是不是“指针类型”
static bool isPointerType(const QString &t)
{
    // 先用一个最粗暴的实现，后续你可以根据自己的 type 格式再细化
    return t.contains('*');
}

// 判断一个事件是不是“给指针重新赋值/初始化”
static bool isPtrAssignEvent(const VarEvent &ev)
{
    const QString &a = ev.action;
    return a.startsWith(QLatin1String("PtrInit"))   // PtrInit*, PtrInitNull, PtrInitAddrOf, PtrInitFromCall, ...
           || a.startsWith(QLatin1String("PtrAlloc"))  // PtrAllocMalloc / PtrAllocNew
           || a == QLatin1String("PtrAlias");          // p = q;
}

// 针对“未初始化分析”这个用途，把一次赋值右值抽象成 mayBeInit / mayBeUninit
static PtrPointsTo evalPtrRhsForUninit(const VarEvent &ev,
                                       const QHash<QString, PtrPointsTo> &curState,
                                       const FuncSummaryMap &funcSummaries)
{
    PtrPointsTo pi;
    pi.hasInfo = true;

    const QString &a = ev.action;

    // p = nullptr / 0
    if (a == QLatin1String("PtrInitNull")) {
        pi.mayBeInit   = true;
        pi.mayBeUninit = false;
        pi.mayNull     = true;
        return pi;
    }

    // p = “未初始化值”（前端直接标成未初始化）
    if (a == QLatin1String("PtrInitUninit")) {
        pi.mayBeInit   = false;
        pi.mayBeUninit = true;
        // 当成垃圾值：既可能 null 也可能 heap
        pi.mayNull     = true;
        pi.mayHeap     = true;
        return pi;
    }

    // p = &x
    if (a == QLatin1String("PtrInitAddrOf")) {
        pi.mayBeInit   = true;
        pi.mayBeUninit = false;
        return pi;
    }

    // p = q
    if (a == QLatin1String("PtrAlias")) {
        // 这里假设 VarEvent 里已有 rhsSymbolId（右值指针 q 的 symbolId）
        if (!ev.rhsSymbolId.isEmpty()) {
            auto it = curState.constFind(ev.rhsSymbolId);
            if (it != curState.constEnd()) {
                return it.value();   // 直接继承 q 的状态
            }
        }
        // 找不到 q：保守，既可能已初始化也可能未初始化
        pi.mayBeInit   = true;
        pi.mayBeUninit = true;
        return pi;
    }

    // p = malloc()/new
    if (a == QLatin1String("PtrAllocMalloc") ||
        a == QLatin1String("PtrAllocNew")) {
        pi.mayBeInit   = true;   // 分配返回的一般当成“已初始化指针”
        pi.mayBeUninit = false;
        pi.mayHeap     = true;
        return pi;
    }

    // p = foo()
    if (a == QLatin1String("PtrInitFromCall")) {
        if (!ev.symbolIdOfCallee.isEmpty()) {
            auto itFun = funcSummaries.constFind(ev.symbolIdOfCallee);
            if (itFun != funcSummaries.constEnd()) {

                const FuncPtrSummary &s = itFun.value();
                qDebug().noquote()
                    << "[evalPtrRhsForUninit]"
                    << "call in func =" << ev.funcName
                    << "calleeSym =" << ev.symbolIdOfCallee
                    << "ret.init =" << s.retPointsTo.mayBeInit
                    << "ret.uninit =" << s.retPointsTo.mayBeUninit;


                // 直接用函数 summary 里的返回值 points-to 信息
                return itFun.value().retPointsTo;
            } else {
                qDebug().noquote()
                    << "[evalPtrRhsForUninit]"
                    << "call in func =" << ev.funcName
                    << "calleeSym =" << ev.symbolIdOfCallee
                    << "NO summary, fallback to unknown";
            }
        }
        // 没有 summary：未知 → 可能 init 也可能 uninit
        pi.mayBeInit   = true;
        pi.mayBeUninit = true;
        return pi;
    }

    // p = 条件表达式 / 其他看不懂的初始化
    if (a == QLatin1String("PtrInitConditional")) {
        // 先用 VarEvent 里预先算好的 rhsPoints 作为基础
        pi = ev.rhsPoints;

        // 无论 rhsPoints 里怎么写，赋值这一步是肯定发生的：
        pi.mayBeInit   = true;
        pi.mayBeUninit = false;

        // 如果前端没填 mayNull / mayHeap，至少保守一点：
        // - 条件表达式里混了 nullptr / 非空 → 可能为空指针
        //   这里简单一点：直接认为“既可能为空，也可能非空”
        if (!pi.hasInfo) {
            pi.hasInfo   = true;
            pi.mayNull   = true;   // 可能为 nullptr
            pi.mayHeap   = true;   // 也可能指向堆
        }

        return pi;
    }

    // 其他看不懂的初始化 p = <unknown>
    if (a == QLatin1String("PtrInit")) {
        // 保守：可能已初始化也可能未初始化
        pi.mayBeInit   = true;
        pi.mayBeUninit = true;
        return pi;
    }

    // 不是赋值事件：返回一个 hasInfo=false，调用方忽略
    pi.hasInfo = false;
    return pi;
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
    Conditional,  // p = ...?...:...
    FromCall
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

                                           qDebug().noquote()
                                               << "[evalPtrRhsExpr CallExpr]"
                                               << "calleeName =" << n->calleeName
                                               << "calleeSym  =" << n->calleeSymbolId
                                               << "varType    =" << n->varType;

                                           if (n->calleeName == "malloc" ||
                                               n->calleeName == "calloc" ||
                                               n->calleeName == "realloc")
                                           {
                                               info.kind = PtrAssignKind::FromMalloc;
                                               return info;
                                           }

                                           // 如果是其它函数调用，而且返回类型是指针
                                           if (!n->calleeSymbolId.isEmpty() && isPointerType(n->varType)) {
                                               info.kind        = PtrAssignKind::FromCall;
                                               info.srcSymbolId = n->calleeSymbolId;  // 记录被调函数的 symbolId

                                               qDebug().noquote()
                                                   << "[evalPtrRhsExpr FromCall]"
                                                   << "calleeName =" << n->calleeName
                                                   << "calleeSym  =" << n->calleeSymbolId
                                                   << "varType    =" << n->varType
                                                   << "→ classify as FromCall";

                                               return info;
                                           }
                                           qDebug().noquote()
                                               << "[evalPtrRhsExpr CallExpr]"
                                               << "calleeName =" << n->calleeName
                                               << "calleeSym  =" << n->calleeSymbolId
                                               << "varType    =" << n->varType
                                               << "→ classify as UNKNOWN (not pointer / no calleeSymbolId)";
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

static PtrPointsTo evalRHSPointsTo(const AstNode *rhs)
{
    PtrExprInfo info = evalPtrRhsExpr(rhs);
    PtrPointsTo res;

    switch(info.kind)
    {
    case PtrAssignKind::Null:
        res.mayNull = true;
        res.mayBeInit = true;
        break;

    case PtrAssignKind::AddrOf:
        res.vars.insert(info.srcSymbolId);
        res.mayBeInit = true;
        break;

    case PtrAssignKind::Copy:
        // 关键点这里不能直接处理
        // 因为其 points-to 来自当时寄存的 state
        // 即 p = q → 等会数据流阶段再处理
        res.vars.insert(info.srcSymbolId);
        res.mayBeInit = true;
        break;
    case PtrAssignKind::FromNew:
    case PtrAssignKind::FromMalloc:
    {
        res.mayHeap = true;
        res.mayBeInit = true;
        QString region = QStringLiteral("heap@") + rhs->astId;
        res.heapRegions.insert(region);
    } break;

    case PtrAssignKind::FromCall:
        // 这里先只标记 callee
        res.vars.insert(info.srcSymbolId);
        res.mayBeInit = true;
        break;

    case PtrAssignKind::Conditional:
        // children[0] 是条件，children[1]/[2] 才是两个分支
        if (rhs && rhs->children.size() == 3) {
            const AstNode *tNode = rhs->children[1].data();
            const AstNode *fNode = rhs->children[2].data();

            PtrPointsTo t = evalRHSPointsTo(tNode);
            PtrPointsTo f = evalRHSPointsTo(fNode);

            // 只要一边有信息，就认为 RHS 有信息
            res.hasInfo = t.hasInfo || f.hasInfo;

            // init / uninit / null / heap 等全部做并集
            res.mayBeInit   = t.mayBeInit   || f.mayBeInit;
            res.mayBeUninit = t.mayBeUninit || f.mayBeUninit;
            res.mayNull     = t.mayNull     || f.mayNull;
            res.mayHeap     = t.mayHeap     || f.mayHeap;

            res.vars        = t.vars;
            res.vars.unite(f.vars);

            res.heapRegions = t.heapRegions;
            res.heapRegions.unite(f.heapRegions);
        }
        break;
    }
    res.hasInfo = true;
    return res;
}

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
                                       const FuncSummaryMap &funcSummaries,
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

        // 把函数中的所有基本块都纳入数据流分析，避免“中间空块”被跳过
        for (const BlockInfo &b : func.blocks) {
            blockIds.insert(b.id);
        }

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
                if (kAssumeParamAndGlobalInitialized) {
                    // 保持当前策略：参数 / 全局默认安全
                    pi.mayBeUninit = false;
                    pi.mayBeInit   = true;
                } else {
                    // 更保守：参数 / 全局入口状态未知
                    pi.mayBeUninit = true;
                    pi.mayBeInit   = true;
                }
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
                        // 针对 nullptr 的两位
                        bool mayNull    = curPi.mayNull;
                        // 很粗略地算一个“可能非空”：指向堆 / 指向某个变量，就算非空
                        bool mayNonNull = curPi.mayHeap || !curPi.vars.isEmpty();

                        // 0：通过参数把“指针地址”传给一个可能初始化它的函数
                        if (ev.action == QStringLiteral("CallPassAddrOf")) {
                            if (!ev.symbolIdOfCallee.isEmpty()) {
                                auto itFun = funcSummaries.constFind(ev.symbolIdOfCallee);
                                if (itFun != funcSummaries.constEnd()) {
                                    const FuncPtrSummary &fs = itFun.value();

                                    auto itPE = fs.paramEffects.constFind(ev.paramIndex);
                                    if (itPE != fs.paramEffects.constEnd()) {
                                        const FuncPtrSummary::ParamPtrEffect &pe = itPE.value();

                                        if (pe.mayInitPointee) {
                                            // 被调用者可能通过 *param 初始化调用者传进来的指针
                                            mayBeUninit = false;
                                            mayBeInit   = true;
                                        }
                                    }
                                }
                            }
                        }

                        // 统一处理“指针赋值/初始化”事件（无论 RHS 多复杂）
                        if (isPtrAssignEvent(ev)) {
                            PtrPointsTo rhsPi = evalPtrRhsForUninit(ev, curState, funcSummaries);

                            if (rhsPi.hasInfo) {
                                // 赋值是强更新：这条路径上，指针的 init 状态等于 RHS 的状态
                                mayBeInit   = rhsPi.mayBeInit;
                                mayBeUninit = rhsPi.mayBeUninit;

                                // 如果你希望在未初始化分析里也顺便维护 points-to，
                                // 可以把整份 ptr 状态也覆盖掉：
                                curPi = rhsPi;
                            } else {
                                // 看不懂 RHS：保守处理
                                mayBeInit   = true;
                                mayBeUninit = true;
                            }
                        }

                        // 1) 指针解引用：根据两位状态来区分
                        if (ev.action == QStringLiteral("PtrDeref")) {
                            // 未初始化解引用
                            if (mayBeUninit) {
                                // 兼容原来的字段：只要有风险就为 true
                                ev.isUninitPtrDeref = true;

                                // 区分“一定未初始化”和“可能未初始化”
                                ev.isDefiniteUninitPtrDeref = !mayBeInit;
                                //   - mayBeUninit == true && mayBeInit == false → 一定未初始化
                                //   - mayBeUninit == true && mayBeInit == true  → 可能未初始化
                            }

                            // 空指针解引用（新逻辑）
                            if (mayNull) {
                                ev.isNullPtrDeref = true;

                                // 如果既可能为 null，又明确有非空来源，就当“可能为空”
                                // 如果只可能为 null，且不是“未初始化垃圾值”，就当“一定为 null”
                                ev.isDefiniteNullPtrDeref = !mayNonNull && !mayBeUninit;
                                //  - mayNull=true, mayNonNull=false, mayBeUninit=false → 一定为空指针
                                //  - 其他情况 → 可能为空指针
                            }
                        }

                        // 写回当前状态
                        curPi.mayBeUninit = mayBeUninit;
                        curPi.mayBeInit   = mayBeInit;
                        curState.insert(ev.symbolId, curPi);

                        qDebug().noquote()
                            << "[markUninit]"
                            << "func =" << ev.funcName
                            << "line =" << ev.line
                            << "action =" << ev.action
                            << "sym =" << ev.symbolId
                            << "after stmt: init =" << curPi.mayBeInit
                            << "uninit =" << curPi.mayBeUninit;
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


// 对单个函数做一次数据流分析，利用已有的 calleeSummaries
static FuncPtrSummary analyzeFuncSummaryOnePass(const ProgramModel &model,
                                                const FunctionInfo &func,
                                                const QVector<VarEvent> &events,
                                                const FuncSummaryMap &calleeSummaries)
{
    FuncPtrSummary sum;
    sum.returnsPointer = false;   // 先默认 false，后面看到 ReturnPtr 再改

    // 1) 建立形参 symbolId -> 下标 的映射
    QHash<QString, int> paramIndexBySym;
    for (int i = 0; i < func.params.size(); ++i) {
        const QString &sid = func.params[i].symbolId;
        if (!sid.isEmpty())
            paramIndexBySym.insert(sid, i);
    }

    // 2) 收集本函数里的事件，按 blockId 分组
    QHash<int, QVector<int>> eventsInBlock;
    bool hasParamOut = false;

    for (int i = 0; i < events.size(); ++i) {
        const VarEvent &ev = events[i];
        if (ev.funcName != func.name)
            continue;
        if (ev.blockId < 0)
            continue;

        eventsInBlock[ev.blockId].push_back(i);

        // 判断是否返回指针
        if (ev.action == QStringLiteral("ReturnPtr")||
            ev.action == QStringLiteral("ReturnPtrFromCall")) {
            sum.returnsPointer = true;
        }

        // ParamPtrInit 说明这个参数的 pointee 可能被初始化
        if (ev.action == QStringLiteral("ParamPtrInit")) {
            hasParamOut = true;
            auto it = paramIndexBySym.constFind(ev.symbolId);
            if (it != paramIndexBySym.constEnd()) {
                int idx = it.value();
                auto &pe = sum.paramEffects[idx];
                pe.mayInitPointee = true;
            }
        }
    }

    // 3) 构建 blockId 集合
    QSet<int> blockIds;
    for (const BlockInfo &b : func.blocks) {
        blockIds.insert(b.id);
    }

    // 如果既不返回指针也没有 ParamOut，可以直接返回空 summary
    if (!sum.returnsPointer && !hasParamOut)
        return sum;

    // 4) 构建 ptrMeta，只挑指针变量
    struct PtrMeta {
        bool isPtr    = false;
        bool isGlobal = false;
        bool isLocal  = false;
        bool isParam  = false;
    };
    QHash<QString, PtrMeta> ptrMeta;

    for (int i = 0; i < events.size(); ++i) {
        const VarEvent &ev = events[i];
        if (ev.funcName != func.name)
            continue;
        if (ev.symbolId.isEmpty())
            continue;
        if (!isPointerType(ev.varType))
            continue;

        PtrMeta &m = ptrMeta[ev.symbolId];
        m.isPtr    = true;
        m.isGlobal = m.isGlobal || ev.isGlobal;
        m.isLocal  = m.isLocal  || ev.isLocal;
        m.isParam  = m.isParam  || ev.isParam;
    }


    // 5) 数据流状态 inState / outState（这一块把你原来 buildLocalFuncSummaries 里的那套搬过来）

    using PtrStateMap = QHash<QString, PtrPointsTo>;   // 或你原来用的 PtrInfo/PtrState 结构

    PtrStateMap baseState;
    for (auto it = ptrMeta.constBegin(); it != ptrMeta.constEnd(); ++it) {
        const QString &sym = it.key();
        const PtrMeta &m   = it.value();

        PtrPointsTo pi;
        if (m.isLocal && !m.isGlobal && !m.isParam) {
            // 局部指针：默认未初始化
            pi.mayBeUninit = true;
            pi.mayBeInit   = false;
        } else {
            // 形参 / 全局，看配置
            if (kAssumeParamAndGlobalInitialized) {
                pi.mayBeUninit = false;
                pi.mayBeInit   = true;
            } else {
                pi.mayBeUninit = true;
                pi.mayBeInit   = true;
            }
        }

        baseState.insert(sym, pi);
    }

    QHash<int, PtrStateMap> inState;
    QHash<int, PtrStateMap> outState;

    // 初始化：所有块 inState/outState 为空
    for (int bid : blockIds) {
        inState[bid]  = baseState;
        outState[bid] = baseState;
    }

    bool changed = true;
    while (changed) {
        changed = false;

        for (int bid : blockIds) {
            PtrStateMap curState = inState[bid];

            // 顺序遍历本块里的事件
            const auto itEvtVec = eventsInBlock.constFind(bid);
            if (itEvtVec != eventsInBlock.constEnd()) {
                const QVector<int> &idxs = itEvtVec.value();
                for (int ei : idxs) {
                    const VarEvent &ev = events[ei];

                    // return funcCall(...)
                    if (ev.action == QStringLiteral("ReturnPtrFromCall")) {
                        sum.returnsPointer = true;

                        PtrPointsTo &ret = sum.retPointsTo;

                        // 把被调函数的返回 summary 合并进来
                        if (!ev.symbolIdOfCallee.isEmpty()) {
                            auto itCal = calleeSummaries.constFind(ev.symbolIdOfCallee);
                            if (itCal != calleeSummaries.constEnd()) {
                                const PtrPointsTo &calleeRet = itCal.value().retPointsTo;

                                ret.hasInfo     = ret.hasInfo || calleeRet.hasInfo;
                                ret.mayBeInit   = ret.mayBeInit   || calleeRet.mayBeInit;
                                ret.mayBeUninit = ret.mayBeUninit || calleeRet.mayBeUninit;
                                ret.mayNull     = ret.mayNull     || calleeRet.mayNull;
                                ret.mayHeap     = ret.mayHeap     || calleeRet.mayHeap;

                                ret.vars.unite(calleeRet.vars);
                                ret.heapRegions.unite(calleeRet.heapRegions);
                            } else {
                                // 没有被调函数 summary：当成“有指针返回，但未知状态”
                                // 可以保持 ret.hasInfo 为 false，让上层走保守 (true,true) 分支
                            }
                        }

                        // 调试一下：
                        qDebug().noquote()
                            << "[Summary ReturnPtrFromCall]"
                            << "func =" << func.name
                            << "callee =" << ev.symbolIdOfCallee
                            << "ret.init =" << ret.mayBeInit
                            << "ret.uninit =" << ret.mayBeUninit
                            << "ret.mayNull =" << ret.mayNull;
                    }

                    auto itPtr = ptrMeta.constFind(ev.symbolId);
                    if (itPtr == ptrMeta.constEnd() || !itPtr->isPtr)
                        continue;

                    // 入口状态：先用 baseState，再用当前块的 curState 覆盖
                    PtrPointsTo basePi = baseState.value(ev.symbolId);
                    PtrPointsTo curPi  = curState.value(ev.symbolId, basePi);

                    bool mayBeInit   = curPi.mayBeInit;
                    bool mayBeUninit = curPi.mayBeUninit;

                    // 指针赋值 / 初始化：强更新
                    if (isPtrAssignEvent(ev)) {
                        PtrPointsTo rhsPi = evalPtrRhsForUninit(ev, curState, calleeSummaries);

                        if (rhsPi.hasInfo) {
                            mayBeInit   = rhsPi.mayBeInit;
                            mayBeUninit = rhsPi.mayBeUninit;
                            curPi       = rhsPi;           // 同时更新 points-to 信息
                        } else {
                            mayBeInit   = true;
                            mayBeUninit = true;
                        }
                    }
                    // return p
                    if (ev.action == QStringLiteral("ReturnPtr")) {
                        sum.returnsPointer = true;

                        PtrPointsTo &ret = sum.retPointsTo;
                        ret.hasInfo = true;

                        ret.mayBeInit   |= curPi.mayBeInit;
                        ret.mayBeUninit |= curPi.mayBeUninit;
                        ret.mayNull     |= curPi.mayNull;
                        ret.mayHeap     |= curPi.mayHeap;

                        ret.vars.unite(curPi.vars);
                        ret.heapRegions.unite(curPi.heapRegions);

                        // ===== 调试日志：看返回时 p 的状态 =====
                        qDebug().noquote()
                            << "[Summary ReturnPtr]"
                            << "func =" << func.name
                            << "line =" << ev.line
                            << "sym =" << ev.symbolId
                            << "curPi.init =" << curPi.mayBeInit
                            << "curPi.uninit =" << curPi.mayBeUninit
                            << "ret.init =" << ret.mayBeInit
                            << "ret.uninit =" << ret.mayBeUninit;

                    }



                    // 更新状态
                    curPi.mayBeInit   = mayBeInit;
                    curPi.mayBeUninit = mayBeUninit;
                    curState.insert(ev.symbolId, curPi);
                }

            }

            if (curState != outState[bid]) {
                outState[bid] = curState;
                changed = true;
            }
        }
    }

    return sum;
}


static bool funcSummaryEqual(const FuncPtrSummary &a,
                             const FuncPtrSummary &b)
{
    if (a.returnsPointer != b.returnsPointer) return false;

    const PtrPointsTo &ap = a.retPointsTo;
    const PtrPointsTo &bp = b.retPointsTo;

    if (ap.hasInfo     != bp.hasInfo)     return false;
    if (ap.mayBeInit   != bp.mayBeInit)   return false;
    if (ap.mayBeUninit != bp.mayBeUninit) return false;
    if (ap.mayHeap     != bp.mayHeap)     return false;
    if (ap.mayNull     != bp.mayNull)     return false;
    if (ap.vars        != bp.vars)        return false;
    if (ap.heapRegions != bp.heapRegions) return false;

    if (a.retAliasParams != b.retAliasParams) return false;

    // 后面如果在 FuncPtrSummary 里再加字段，也一起比一下
    return true;
}

static FuncSummaryMap buildFuncSummariesBottomUp(const ProgramModel &model,
                                                 const QVector<VarEvent> &events)
{
    qDebug().noquote()
        << "===== BuildFuncSummariesBottomUp: functions =" << model.functions.size()
        << " events =" << events.size() << " =====";

    FuncSummaryMap result;

    FuncIndex funcIndex;
    CallGraph callGraph;
    buildFuncIndexAndCallGraph(model, funcIndex, callGraph);

    QVector<QVector<QString>> sccTopo = computeSccTopoOrder(callGraph);

    // 注意：callGraph 是 caller -> callee，
    // topo 里是“从 root 到 leaf”（main 在前，叶子在后），
    // 而我们要自底向上，所以要 **逆序** 处理：
    for (int gi = sccTopo.size() - 1; gi >= 0; --gi) {
        const QVector<QString> &group = sccTopo[gi];

        if (group.size() == 1) {
            // ===== 非递归函数 =====
            const QString &sym = group[0];
            int idx = funcIndex.value(sym, -1);
            if (idx < 0) continue;

            const FunctionInfo &func = model.functions[idx];

            qDebug().noquote()
                << "[FuncSummary] non-recursive func =" << func.name
                << "(" << sym << ")";

            FuncPtrSummary sum = analyzeFuncSummaryOnePass(model, func, events, result);

            const PtrPointsTo &rp = sum.retPointsTo;
            qDebug().noquote()
                << "    ret: hasInfo=" << rp.hasInfo
                << " init=" << rp.mayBeInit
                << " uninit=" << rp.mayBeUninit
                << " mayNull=" << rp.mayNull
                << " mayHeap=" << rp.mayHeap;

            if (!func.symbolId.isEmpty())
                result.insert(func.symbolId, sum);

        } else {
            // ===== 递归 / 互相递归 SCC：迭代到固定点 =====
            bool changed = true;
            int iter = 0;

            QStringList syms;
            for (const QString &s : group) syms << s;
            qDebug().noquote()
                << "[FuncSummary] SCC group start:" << syms.join(QStringLiteral(", "));

            while (changed) {
                changed = false;
                iter++;

                qDebug().noquote()
                    << "  SCC iteration" << iter;

                for (const QString &sym : group) {
                    int idx = funcIndex.value(sym, -1);
                    if (idx < 0) continue;

                    const FunctionInfo &func = model.functions[idx];

                    FuncPtrSummary oldSum = result.value(sym, FuncPtrSummary{});
                    FuncPtrSummary newSum = analyzeFuncSummaryOnePass(model, func, events, result);

                    const PtrPointsTo &op = oldSum.retPointsTo;
                    const PtrPointsTo &np = newSum.retPointsTo;
                    qDebug().noquote()
                        << "    func =" << func.name << "(" << sym << ")"
                        << " old[hasInfo,init,uninit,mayNull]=("
                        << op.hasInfo << "," << op.mayBeInit << ","
                        << op.mayBeUninit << "," << op.mayNull << ")"
                        << " new[hasInfo,init,uninit,mayNull]=("
                        << np.hasInfo << "," << np.mayBeInit << ","
                        << np.mayBeUninit << "," << np.mayNull << ")";

                    if (!func.symbolId.isEmpty()) {
                        if (!funcSummaryEqual(oldSum, newSum)) {
                            result.insert(func.symbolId, newSum);
                            changed = true;
                        }
                    }
                }
            }

            qDebug().noquote()
                << "[FuncSummary] SCC group fixed-point after" << iter << "iterations.";


        }
    }

    return result;
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
                    // -------- 0) Return 语句：如果返回的是指针变量，记录一个事件 --------
                    if (n.kind == "ReturnStmt") {
                        qDebug().noquote()
                            << "[ReturnStmt AST]"
                            << "func =" << func.name
                            << "blockId =" << block.id
                            << "stmtLine =" << stmtLine
                            << "children.size =" << n.children.size();

                        if (!n.children.isEmpty()) {

                            const AstNode *raw = n.children.first().data();
                            const AstNode *retExpr = stripCastsAndParens(raw);

                            qDebug().noquote()
                                << "  raw.kind =" << (raw ? raw->kind : "<null>")
                                << "raw.varType =" << (raw ? raw->varType : "<null>")
                                << "raw.calleeName =" << (raw ? raw->calleeName : "<null>")
                                << "raw.calleeSym =" << (raw ? raw->calleeSymbolId : "<null>");

                            qDebug().noquote()
                                << "  stripped.kind =" << (retExpr ? retExpr->kind : "<null>")
                                << "stripped.varType =" << (retExpr ? retExpr->varType : "<null>")
                                << "stripped.calleeName =" << (retExpr ? retExpr->calleeName : "<null>")
                                << "stripped.calleeSym =" << (retExpr ? retExpr->calleeSymbolId : "<null>");


                            // ① 返回的是某个指针变量：保持现有逻辑
                            const AstNode *ptr = findFirstPtrRef(retExpr);
                            if (ptr && !ptr->symbolId.isEmpty()) {
                                VarEvent ev;
                                ev.funcName = func.name;
                                ev.blockId  = block.id;
                                ev.line     = stmtLine;
                                attachThreadInfo(ev, func, threadsByFunc);
                                ev.varName  = ptr->varName;
                                ev.symbolId = ptr->symbolId;
                                ev.varType  = ptr->varType;
                                ev.isGlobal = ptr->isGlobal;
                                ev.isLocal  = ptr->isLocal;
                                ev.isParam  = ptr->isParam;
                                ev.action   = QStringLiteral("ReturnPtr");
                                ev.detail   = QStringLiteral("返回指针 %1").arg(ptr->varName);
                                ev.code     = stmtCode;
                                out.events.push_back(std::move(ev));
                            }
                            // 返回的是“函数调用结果”，而且返回类型是指针：
                            // ② return make_ptr_maybe(flag);  这一类：直接返回一个“指针类型的函数调用结果”
                            else if (retExpr &&
                                       retExpr->kind == "CallExpr" &&
                                       !retExpr->calleeSymbolId.isEmpty() &&
                                       isPointerType(retExpr->varType))
                            {
                                qDebug().noquote()
                                    << "[build VarEvent ReturnPtrFromCall]"
                                    << "func =" << func.name
                                    << "line =" << stmtLine
                                    << "calleeSym =" << retExpr->calleeSymbolId
                                    << "varType =" << retExpr->varType;

                                VarEvent ev;
                                ev.funcName = func.name;
                                ev.blockId  = block.id;
                                ev.line     = stmtLine;
                                attachThreadInfo(ev, func, threadsByFunc);

                                ev.action           = QStringLiteral("ReturnPtrFromCall");
                                ev.symbolIdOfCallee = retExpr->calleeSymbolId;   // c:@F@make_ptr_maybe#b#
                                ev.varType          = retExpr->varType;          // "int *"
                                ev.detail           = QStringLiteral("返回调用结果");
                                ev.code             = stmtCode;

                                out.events.push_back(std::move(ev));
                            }
                        }
                    }
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

                                // === 构造这次解引用的别名链 ======================
                                {
                                    QSet<QString> aliasSyms;

                                    // 1) 先把自己放进去
                                    if (!ptr->symbolId.isEmpty())
                                        aliasSyms.insert(ptr->symbolId);

                                    // 2) 当前已知的 points-to 目标
                                    PtrPointsTo pi = state.ptrPoints.value(ptr->symbolId, PtrPointsTo{});
                                    for (const QString &v : pi.vars)
                                        aliasSyms.insert(v);

                                    // 3) 再把“指向同一批目标”的其它指针变量也加进来
                                    if (!pi.vars.isEmpty()) {
                                        for (auto it = state.ptrPoints.constBegin();
                                             it != state.ptrPoints.constEnd(); ++it) {
                                            const QString &otherSym = it.key();
                                            if (otherSym == ptr->symbolId)
                                                continue;

                                            const PtrPointsTo &opi = it.value();
                                            bool share = false;
                                            for (const QString &v : pi.vars) {
                                                if (opi.vars.contains(v)) {
                                                    share = true;
                                                    break;
                                                }
                                            }
                                            if (share)
                                                aliasSyms.insert(otherSym);
                                        }
                                    }

                                    QStringList aliasList;
                                    aliasList.reserve(aliasSyms.size());
                                    for (const QString &s : aliasSyms)
                                        aliasList << s;

                                    ev.ptrAliasSymbolIds = aliasList;
                                }
                                // ======================================================

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

                                {
                                    QSet<QString> aliasSyms;

                                    if (!ptr->symbolId.isEmpty())
                                        aliasSyms.insert(ptr->symbolId);

                                    PtrPointsTo pi = state.ptrPoints.value(ptr->symbolId, PtrPointsTo{});
                                    for (const QString &v : pi.vars)
                                        aliasSyms.insert(v);

                                    if (!pi.vars.isEmpty()) {
                                        for (auto it = state.ptrPoints.constBegin();
                                             it != state.ptrPoints.constEnd(); ++it) {
                                            const QString &otherSym = it.key();
                                            if (otherSym == ptr->symbolId)
                                                continue;

                                            const PtrPointsTo &opi = it.value();
                                            bool share = false;
                                            for (const QString &v : pi.vars) {
                                                if (opi.vars.contains(v)) {
                                                    share = true;
                                                    break;
                                                }
                                            }
                                            if (share)
                                                aliasSyms.insert(otherSym);
                                        }
                                    }

                                    QStringList aliasList;
                                    aliasList.reserve(aliasSyms.size());
                                    for (const QString &s : aliasSyms)
                                        aliasList << s;

                                    ev.ptrAliasSymbolIds = aliasList;
                                }

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
                                PtrPointsTo rhsPi = evalRHSPointsTo(&rhsNode);

                                VarEvent ev;
                                ev.funcName = func.name;
                                ev.blockId  = blockId;
                                ev.line     = stmtLine;
                                ev.astId    = stmt.astId;
                                attachThreadInfo(ev, func, threadsByFunc);
                                ev.varName  = ptrLhs->varName;
                                ev.symbolId = ptrLhs->symbolId;
                                ev.varType  = ptrLhs->varType;
                                ev.isGlobal = ptrLhs->isGlobal;
                                ev.isLocal  = ptrLhs->isLocal;
                                ev.isParam  = ptrLhs->isParam;
                                ev.code     = stmtCode;
                                ev.rhsSymbolId = srcSym;        // 需要的地方继续用
                                ev.rhsPoints   = rhsPi;         // ★ 新增：记录右值 points-to

                                auto &ptrInfo = state.ptrPoints[ptrLhs->symbolId];
                                // 先把 runtime 状态同步成 rhsPi
                                ptrInfo = rhsPi;
                                ptrInfo.hasInfo = true;          // 从这一刻开始有信息了

                                // 这里不再在收集事件阶段改 ptrInfo 的具体内容，
                                // 只做 UI（detail / lastValueHint）和记录“右值是谁”
                                switch (pk) {
                                case PtrAssignKind::AddrOf:
                                    ev.action = QStringLiteral("PtrInitAddrOf");
                                    ev.detail = QStringLiteral("指针 %1 = &%2")
                                                    .arg(ptrLhs->varName,
                                                         srcName);
                                    vs.lastValueHint =
                                        QStringLiteral("addrOf %1").arg(srcName);

                                    // 右值变量（&谁）
                                    ev.rhsSymbolId = srcSym;
                                    break;

                                case PtrAssignKind::Null:
                                    ev.action = QStringLiteral("PtrInitNull");
                                    ev.detail = QStringLiteral("指针 %1 赋值为 nullptr")
                                                    .arg(ptrLhs->varName);
                                    vs.lastValueHint = QStringLiteral("null");
                                    // 不需要额外的 rhsSymbolId
                                    break;

                                case PtrAssignKind::Copy:
                                    ev.action = QStringLiteral("PtrAlias");
                                    ev.detail = QStringLiteral("指针 %1 = %2 (别名)")
                                                    .arg(ptrLhs->varName,
                                                         srcName);
                                    vs.lastValueHint =
                                        QStringLiteral("alias of %1").arg(srcName);

                                    // 右值指针（alias 源头）
                                    ev.rhsSymbolId = srcSym;
                                    break;

                                case PtrAssignKind::FromMalloc:
                                    ev.action = QStringLiteral("PtrAllocMalloc");
                                    ev.detail = QStringLiteral("指针 %1 从 malloc/calloc/realloc 分配")
                                                    .arg(ptrLhs->varName);
                                    vs.lastValueHint = QStringLiteral("from malloc");
                                    // heap region 具体内容放在数据流 / summary 那一侧处理
                                    break;

                                case PtrAssignKind::FromNew:
                                    ev.action = QStringLiteral("PtrAllocNew");
                                    ev.detail = QStringLiteral("指针 %1 从 new 表达式分配")
                                                    .arg(ptrLhs->varName);
                                    vs.lastValueHint = QStringLiteral("from new");
                                    break;

                                case PtrAssignKind::Conditional:
                                    ev.action = QStringLiteral("PtrInitConditional");
                                    ev.detail = QStringLiteral("指针 %1 通过条件表达式初始化")
                                                    .arg(ptrLhs->varName);
                                    vs.lastValueHint = QStringLiteral("conditional init");
                                    break;

                                case PtrAssignKind::FromCall:
                                    ev.action = QStringLiteral("PtrInitFromCall");
                                    ev.detail = QStringLiteral("指针 %1 = 调用函数返回值")
                                                    .arg(ptrLhs->varName);
                                    vs.lastValueHint = QStringLiteral("from call");

                                    // 记录被调用函数的 symbolId，后面 summary 用
                                    ev.symbolIdOfCallee = srcSym;

                                    qDebug().noquote()
                                        << "[build VarEvent PtrInitFromCall]"
                                        << "func =" << func.name
                                        << "line =" << stmtLine
                                        << "lhsSym =" << ev.symbolId
                                        << "calleeSym =" << ev.symbolIdOfCallee;

                                    break;

                                case PtrAssignKind::Unknown:
                                default:
                                    // 看不懂的 RHS，统一当成“未知初始化”
                                    ev.action = QStringLiteral("PtrInit");
                                    ev.detail = QStringLiteral("指针 %1 以未知方式初始化")
                                                    .arg(ptrLhs->varName);
                                    vs.lastValueHint = QStringLiteral("unknown init");
                                    break;
                                }


                                out.events.push_back(std::move(ev));
                            } else {
                                // === 新增：检测 *param = ... 这种“通过参数输出指针”的写法 ===
                                const AstNode *lhsStripped = stripCastsAndParens(&lhsNode);
                                if (lhsStripped &&
                                    lhsStripped->kind == "UnaryOperator" &&
                                    lhsStripped->op == "*" &&
                                    !lhsStripped->children.isEmpty())
                                {
                                    const AstNode *inner =
                                        stripCastsAndParens(lhsStripped->children.first().data());

                                    // inner 是一个“指针形参”，例如 int **pp
                                    if (inner &&
                                        inner->kind == "DeclRefExpr" &&
                                        inner->isParam &&
                                        isPointerType(inner->varType))
                                    {
                                        QString srcName;
                                        QString srcSym;
                                        PtrAssignKind pk =
                                            classifyPointerRHS(rhsNode, srcName, srcSym);

                                        // 只在 RHS 明确是指针来源时认为是“初始化调用者指针”
                                        if (pk != PtrAssignKind::Unknown) {
                                            VarEvent ev;
                                            ev.funcName = func.name;
                                            ev.blockId  = blockId;
                                            ev.line     = stmtLine;
                                            attachThreadInfo(ev, func, threadsByFunc);
                                            ev.varName  = inner->varName;      // 形参名，比如 pp
                                            ev.symbolId = inner->symbolId;     // 形参 symbolId
                                            ev.varType  = inner->varType;
                                            ev.isGlobal = inner->isGlobal;
                                            ev.isLocal  = inner->isLocal;
                                            ev.isParam  = inner->isParam;
                                            ev.action   = QStringLiteral("ParamPtrInit");
                                            ev.detail   = QStringLiteral("通过 *%1 初始化调用者指针")
                                                            .arg(inner->varName);
                                            ev.code     = stmtCode;
                                            out.events.push_back(std::move(ev));
                                        }
                                    }
                                }
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

                        // 4.x 识别把“指针地址”作为参数传递的情况：foo(&p)
                        int argIndex = 0;

                        // 注意：children[0] 是 callee（init_out），从 children[1] 起才是实参
                        for (int childIdx = 0; childIdx < n.children.size(); ++childIdx) {

                            // 对 CallExpr / CXXMemberCallExpr，约定 child[0] 为 callee，跳过
                            if (childIdx == 0)
                                continue;

                            const auto &child = n.children[childIdx];
                            const AstNode *arg = stripCastsAndParens(child.data());
                            if (!arg) {
                                ++argIndex;              // 这是第 argIndex 个“实参”，即便解析失败也要占位
                                continue;
                            }

                            // 形如：&p
                            if (arg->kind == "UnaryOperator" &&
                                arg->op == "&" &&
                                !arg->children.isEmpty())
                            {
                                const AstNode *inner =
                                    stripCastsAndParens(arg->children.first().data());

                                if (inner &&
                                    inner->kind == "DeclRefExpr" &&
                                    isPointerType(inner->varType))
                                {
                                    VarEvent ev;
                                    ev.funcName = func.name;
                                    ev.blockId  = blockId;
                                    ev.line     = stmtLine;
                                    attachThreadInfo(ev, func, threadsByFunc);

                                    ev.varName  = inner->varName;
                                    ev.symbolId = inner->symbolId;
                                    ev.varType  = inner->varType;
                                    ev.isGlobal = inner->isGlobal;
                                    ev.isLocal  = inner->isLocal;
                                    ev.isParam  = inner->isParam;

                                    ev.action           = QStringLiteral("CallPassAddrOf");
                                    ev.symbolIdOfCallee = n.calleeSymbolId;
                                    ev.paramIndex       = argIndex;   // 现在是正确的“第几个实参”（0-based）

                                    ev.detail = QStringLiteral("将指针 %1 的地址作为第 %2 个参数传给 %3")
                                                    .arg(inner->varName)
                                                    .arg(argIndex + 1)
                                                    .arg(calleeName);
                                    ev.code = stmtCode;

                                    out.events.push_back(std::move(ev));
                                }
                            }

                            ++argIndex;   // 只对“实参”递增，而不是对每个 child 递增
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

    qDebug().noquote() << "===== Step 1.x: buildFuncSummariesBottomUp =====";

    // 1.x) 基于调用图 + SCC 自底向上构建最终的函数 summary
    out.funcSummaries = buildFuncSummariesBottomUp(model, out.events);

    // 1.5) 根据 MutexLock/MutexUnlock 标记 underLock / heldMutexes
    markLockRegions(model, out.events);

    // 1.6) 基于 CFG 分析哪些指针解引用“可能未初始化”
    markUninitializedPtrDerefs(model, out.funcSummaries, out.events);

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
