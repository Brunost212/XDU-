// analysis/Analyzer.h
#ifndef ANALYZER_H
#define ANALYZER_H

#include "analysis/JsonModel.h"

namespace analysis {

// 针对每个变量的汇总信息
struct VarSummary {
    QString symbolId;
    QString name;      // 变量名
    QString scopeDesc; // 作用域描述：全局/参数/局部/未知

    bool everAssigned = false;
    bool everUsed     = false;

    bool hasFirstAssign = false;
    bool hasFirstUse    = false;
    VarEvent firstAssign;
    VarEvent firstUse;
};

// 针对函数返回值/参数的指针总结（先做一个简单版）
struct FuncPtrSummary {
    // 这个函数是否返回指针类型
    bool returnsPointer = false;

    // 返回的指针在所有返回路径上的初始化情况：
    bool retMayBeInit   = false;  // 有路径上是已初始化（非未初始化）
    bool retMayBeUninit = false;  // 有路径上是未初始化

    // 如果返回值可能是堆上的指针（malloc/new）
    bool retMayBeHeap   = false;
    // 可能为 nullptr/0
    bool retMayBeNull   = false;

    // 返回值可能别名的参数 symbolId 集合
    QSet<QString> retAliasParams;

    // === 新增：参数对“调用者指针”的影响（按形参下标） ===
    struct ParamPtrEffect {
        bool mayInitPointee = false;  // 这个参数是否“会通过 *param 初始化调用者传进来的指针”
    };
    QHash<int, ParamPtrEffect> paramEffects; // key = 形参下标 0-based
};

// 整个 TU 的函数 summary：key = function.symbolId
using FuncSummaryMap = QHash<QString, FuncPtrSummary>;

// 整体分析输出：事件 + 按变量汇总
struct AnalysisOutput {
    QVector<VarEvent> events;             // 所有细粒度事件（Use/Assign/Call...）
    QHash<QString, VarSummary> vars;      // key = symbolId
    FuncSummaryMap funcSummaries;
};

// 主入口：对一个 ProgramModel 做分析，返回 AnalysisOutput
AnalysisOutput analyzeProgram(const ProgramModel &model);

} // namespace analysis

#endif
