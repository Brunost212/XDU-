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

// 针对函数返回值/参数的指针总结
struct FuncPtrSummary {
    // 这个函数是否返回指针类型
    bool returnsPointer = false;

    // 函数返回值的 points-to 抽象（跨所有 return 路径合并）
    PtrPointsTo retPointsTo;

    // 返回值可能别名的参数 symbolId 集合（可选：后面可以用来优化别名）
    QSet<QString> retAliasParams;

    // === 参数对“调用者指针”的影响（按形参下标） ===
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
