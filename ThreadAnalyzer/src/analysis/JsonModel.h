// analysis/JsonModel.h
#ifndef JSONMODEL_H
#define JSONMODEL_H

#include <QVector>
#include <QHash>
#include <QSharedPointer>
#include <QString>
#include <QSet>

class QJsonDocument;

namespace analysis {

// 一棵 AST 树的节点
struct AstNode {
    QString astId;
    QString kind;
    QString code;
    QString file;
    QString op;
    QString calleeName;
    QString calleeSymbolId;
    QString memberName;
    int line = 0;
    int col  = 0;

    // 和符号关联的信息（analysis.json 里有就抄，有就用）
    QString symbolId;
    QString varName;
    QString varType;
    bool isGlobal = false;
    bool isLocal  = false;
    bool isParam  = false;

    QVector<QSharedPointer<AstNode>> children;
};

struct ThreadOp {
    QString api;                 // "pthread_create" / "std::thread" 等
    QString kind;                // "createThread" 之类
    QString className;           // 对 std::thread 来说是 "thread"

    QString threadVarName;       // t1 / t2 / pthread_t 变量名
    QString threadVarType;       // 变量类型
    int     threadVarId = -1;    // 插件那边的 varId，如果有的话

    QString startFuncName;       // 线程入口函数名
    QString startFuncSymbolId;   // 入口函数的 symbolId（跟 functions[].symbolId 对得上）
};

// 一条语句
struct StmtInfo {
    QString astId;
    QString kind;
    QString code;
    QString file;
    int line = 0;
    int col  = 0;

    // 这条语句对应的 AST 子树
    QSharedPointer<AstNode> tree;

    // 这条语句上收集到的线程操作
    QVector<ThreadOp> threadOps;
};

// 一个 basic block
struct BlockInfo {
    int id = -1;
    QVector<int> predecessors;
    QVector<int> successors;
    QVector<StmtInfo> stmts;
};

// ----------------- 简单变量状态 & 事件 -----------------

// 单个变量的抽象状态（这里只做示例，可以以后慢慢加字段）
struct VarState {
    bool everAssigned = false;  // 是否出现过赋值
    bool everRead     = false;  // 是否出现过使用
    bool everFreed    = false;  // 对指针而言，是否被 free / delete 过

    QString lastValueHint;      // 简单记录一下最近一次赋值的大概信息（例如 "from malloc"）
};

// 每个指针变量当前的“指向信息”摘要
struct PtrPointsTo {
    bool hasInfo   = false;        // 是否已经有有意义的指向信息
    bool mayNull   = false;        // 可能为 nullptr/0
    bool mayHeap   = false;        // 可能指向堆内存（malloc/new）
    bool mayBeUninit = false;        // 可能“尚未初始化”（专门给 CFG 流分析用）
    bool mayBeInit   = false;   // 存在路径上已初始化
    QSet<QString> vars;            // 可能指向的变量 symbolId 集合（&x / 指向别的指针等）

    bool operator==(const PtrPointsTo &other) const {
        return hasInfo     == other.hasInfo
               && mayNull     == other.mayNull
               && mayHeap     == other.mayHeap
               && mayBeUninit == other.mayBeUninit
               && mayBeInit    == other.mayBeInit
               && vars        == other.vars;
    }
    bool operator!=(const PtrPointsTo &other) const {
        return !(*this == other);
    }
};

// 当前分析过程中的状态（按函数/按路径都可以用这个）
struct AnalysisState {
    // key 用 symbolId，避免重名问题
    QHash<QString, VarState> vars;

    // 新增：每个指针变量的 points-to 信息
    QHash<QString, PtrPointsTo> ptrPoints;
};

// 记录“我关心的东西发生了什么变化”的事件
struct VarEvent {
    QString funcName;
    int     blockId = -1;
    int     line    = 0;

    QString varName;
    QString varType;
    QString symbolId;

    //变量作用域
    bool isGlobal = false;
    bool isLocal = false;
    bool isParam = false;

    QString action;   // "Assign" / "Use" / "Free" 等
    QString detail;   // 额外描述（例如赋值表达式的 code）

    QString code;     // 整条语句源码

    // 线程维度（可选，如果该函数是某个线程入口）
    QString threadVarName;    // 线程对象/句柄变量名，比如 t1 / tid
    QString threadApi;        // "pthread_create" / "std::thread" 等
    QString threadEntryName;  // 线程入口函数名，比如 worker_param_ptr

    // 这次访问是否在某个锁区间内，以及当前持有哪些互斥量
    bool underLock = false;
    QStringList heldMutexes;  // 保存 mutex 的 symbolId 或名字，这里先用 symbolId

    // 指针分析：这次解引用是否“可能未初始化”
    bool isUninitPtrDeref = false;
    bool isDefiniteUninitPtrDeref = false;
};

// 变量信息（可以来自 globalVars / params / localVars）
struct VarInfo {
    QString name;
    QString symbolId;
    QString type;
    QString file;
    int line = 0;
    bool isGlobal = false;
    bool isParam  = false;
    bool isLocal  = false;
};

// 函数信息
struct FunctionInfo {
    QString name;
    QString symbolId;
    QString file;
    int line = 0;

    QVector<VarInfo> params;
    QVector<VarInfo> localVars;
    QVector<BlockInfo> blocks;
};

// 整个 TU 的模型
struct ProgramModel {
    QVector<FunctionInfo> functions;
    QVector<VarInfo>      globalVars;

    // 快速索引：astId -> 节点； symbolId -> 变量
    QHash<QString, QSharedPointer<AstNode>> astIndex;
    QHash<QString, VarInfo> varsBySymbolId;
};

// 从 QJsonDocument 构建 ProgramModel
bool buildProgramModel(const QJsonDocument &doc, ProgramModel &out);

// 直接从文件 path 加载
bool loadProgramModelFromFile(const QString &path, ProgramModel &out);

} // namespace analysis

#endif // JSONMODEL_H
