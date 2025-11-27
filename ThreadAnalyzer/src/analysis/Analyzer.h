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

// 整体分析输出：事件 + 按变量汇总
struct AnalysisOutput {
    QVector<VarEvent> events;             // 所有细粒度事件（Use/Assign/Call...）
    QHash<QString, VarSummary> vars;      // key = symbolId
};

// 主入口：对一个 ProgramModel 做分析，返回 AnalysisOutput
AnalysisOutput analyzeProgram(const ProgramModel &model);

} // namespace analysis

#endif
