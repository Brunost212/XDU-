#include "mainwindow.h"
#include "ui_mainwindow.h"

#include "io/ProjectScan.h"
#include "io/Toolchain.h"
#include "util/Log.h"

#include "analysis/JsonModel.h"
#include "analysis/Analyzer.h"

#include <QDir>
#include <QFileDialog>
#include <QFileInfo>
#include <QDirIterator>
#include <QPointer>
#include <QApplication>
#include <QDesktopServices>
#include <QClipboard>
#include <QUrl>
#include <QFileSystemModel>


MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::MainWindow)
    , m_model(new QFileSystemModel(this))
{
    ui->setupUi(this);

    // 文件浏览器只关心文件名等，隐藏 size/date 列
    ui->fileBrowser->setColumnHidden(3, true);

    // 配置文件系统模型：只显示 C/C++ 源文件
    m_model->setRootPath(QString());
    m_model->setNameFilters(QStringList() << "*.c"
                                          << "*.cpp"
                                          << "*.cc");
    m_model->setNameFilterDisables(false);

    // 日志：让 util::log() 输出到右侧 logBrowser
    QPointer<MainWindow> self(this);
    util::setLogger([self](util::Level lv, const QString &s) {
        if (!self)
            return;
        QMetaObject::invokeMethod(
            self,
            [self, lv, s] {
                if (!self)
                    return;
                switch (lv) {
                case util::Level::Info:
                    self->log(s);
                    break;
                case util::Level::Warn:
                    self->log(QStringLiteral("[WARN]  ") + s);
                    break;
                case util::Level::Error:
                    self->log(QStringLiteral("[ERROR] ") + s);
                    break;
                }
            },
            Qt::QueuedConnection);
    });
}

MainWindow::~MainWindow()
{
    delete ui;
}

void MainWindow::log(const QString &msg)
{
    ui->logBrowser->append(msg);
    QApplication::processEvents();
}

void MainWindow::logStmtSummaryEvent(const analysis::VarEvent &ev)
{
    log(QString());
    // 头部标签
    log(tr("【语句摘要】"));

    // 基本位置信息：函数 / block / 行号
    log(tr("  函数 %1, B%2, 行 %3")
            .arg(ev.funcName)
            .arg(ev.blockId)
            .arg(ev.line));

    // 原始语句源码
    if (!ev.code.isEmpty()) {
        log(tr("  语句: %1").arg(ev.code));
    }

    // 详细解释（你在 Analyzer 里根据 ExprSummary 拼的字符串）
    if (!ev.detail.isEmpty()) {
        log(tr("  说明: %1").arg(ev.detail));
    }

}

// 变量级别的摘要 / 简单告警
void MainWindow::logAnalysisSummary(MainWindow *self,
                                    const analysis::AnalysisOutput &out,
                                    const QString &sourceFile)
{
    if (!self) return;

    self->log(self->tr("===== 文件 %1 的变量总结 / 简单告警 =====").arg(sourceFile));

    for (auto it = out.vars.constBegin(); it != out.vars.constEnd(); ++it) {
        const analysis::VarSummary &sum = it.value();

        // 只读不写：可能未初始化使用
        if (sum.everUsed && !sum.everAssigned && sum.hasFirstUse) {
            const analysis::VarEvent &ev = sum.firstUse;
            const QString msg = self->tr(
                                        "[可能未初始化使用] %1 (%2, symbol=%3) 在函数 %4, B%5, 行 %6 首次被读取，之前未发现赋值")
                                    .arg(sum.name)
                                    .arg(sum.scopeDesc)
                                    .arg(sum.symbolId)
                                    .arg(ev.funcName)
                                    .arg(ev.blockId)
                                    .arg(ev.line);
            self->log(msg);
        }

        // 只写不读：可能赋值未使用
        if (sum.everAssigned && !sum.everUsed && sum.hasFirstAssign) {
            const analysis::VarEvent &ev = sum.firstAssign;
            const QString msg = self->tr(
                                        "[可能赋值未使用] %1 (%2, symbol=%3) 在函数 %4, B%5, 行 %6 首次被赋值，此后未发现读取")
                                    .arg(sum.name)
                                    .arg(sum.scopeDesc)
                                    .arg(sum.symbolId)
                                    .arg(ev.funcName)
                                    .arg(ev.blockId)
                                    .arg(ev.line);
            self->log(msg);
        }
    }
}

// 细粒度事件列表
void MainWindow::logAnalysisEvents(MainWindow *self,
                                   const analysis::AnalysisOutput &out,
                                   const QString &sourceFile)
{
    if (!self) return;

    for (const analysis::VarEvent &ev : out.events) {

        // ① 新增：专门处理“整条语句摘要”事件
        if (ev.action == QStringLiteral("StmtSummary")) {
            self->logStmtSummaryEvent(ev);
            continue;   // 不再走下面通用格式
        }

        // ② 原来的通用事件打印逻辑保持不变
        QString threadPrefix;
        if (!ev.threadVarName.isEmpty()) {
            threadPrefix = self->tr("线程 %1(%2, entry=%3), ")
                               .arg(ev.threadVarName,
                                    ev.threadApi.isEmpty()
                                        ? QStringLiteral("?")
                                        : ev.threadApi,
                                    ev.threadEntryName.isEmpty()
                                        ? ev.funcName  // 没有就用当前函数名兜底
                                        : ev.threadEntryName);
        }

        const QString line = self->tr(
                                     "%1函数 %2, B%3, 行 %4: [%5] %6 (symbol=%7) -- %8\n    语句: %9")
                                 .arg(threadPrefix)
                                 .arg(ev.funcName)
                                 .arg(ev.blockId)
                                 .arg(ev.line)
                                 .arg(ev.action)
                                 .arg(ev.varName.isEmpty()
                                          ? QStringLiteral("<无>")
                                          : ev.varName)
                                 .arg(ev.symbolId)
                                 .arg(ev.detail)
                                 .arg(ev.code);
        self->log(line);
    }
}

// 线程创建信息
void MainWindow::logThreadSummary(MainWindow *self,
                                  const analysis::ProgramModel &model,
                                  const QString &sourceFile)
{
    if (!self) return;

    self->log(self->tr("===== 文件 %1 的线程创建信息 =====").arg(sourceFile));

    for (const auto &func : model.functions) {
        for (const auto &block : func.blocks) {
            for (const auto &stmt : block.stmts) {
                for (const auto &op : stmt.threadOps) {

                    const QString threadName =
                        op.threadVarName.isEmpty()
                            ? self->tr("<匿名线程>")
                            : op.threadVarName;

                    const QString api =
                        op.api.isEmpty()
                            ? QStringLiteral("<unknown>")
                            : op.api;

                    const QString entryName =
                        op.startFuncName.isEmpty()
                            ? self->tr("<未知入口函数>")
                            : op.startFuncName;

                    const QString msg =
                        self->tr("函数 %1, B%2, 行 %3: 通过 %4 创建线程 %5，入口函数 %6 (symbol=%7)")
                            .arg(func.name)
                            .arg(block.id)
                            .arg(stmt.line)
                            .arg(api)
                            .arg(threadName)
                            .arg(entryName)
                            .arg(op.startFuncSymbolId);

                    self->log(msg);
                }
            }
        }
    }
}

void MainWindow::logThreadVarLockSummary(const analysis::AnalysisOutput &out)
{
    struct ThreadVarAccess {
        bool hasLocked   = false;
        bool hasUnlocked = false;
        bool isGlobal = false;
        bool isLocal = false;
        bool isParam = false;
        QString varName;
        QString varType;
        QString symbolId;

        QVector<const analysis::VarEvent*> lockedEvents;
        QVector<const analysis::VarEvent*> unlockedEvents;
    };

    QHash<QString, ThreadVarAccess> perThreadVar;

    for (const analysis::VarEvent &ev : out.events) {

        if (ev.symbolId.isEmpty())
            continue;

        // 只关心真正的变量访问事件
        if (!(ev.action == "Use" ||
              ev.action == "PtrDeref" ||
              ev.action == "Assign" ||
              ev.action.startsWith("Ptr")))
            continue;

        // 要有线程信息
        if (ev.threadVarName.isEmpty())
            continue;

        QString key = ev.threadVarName + "|" + ev.varName + "|" + ev.symbolId;

        auto &acc = perThreadVar[key];
        acc.varName = ev.varName;
        acc.varType = ev.varType;
        acc.symbolId = ev.symbolId;
        acc.isGlobal = ev.isGlobal;
        acc.isLocal = ev.isLocal;
        acc.isParam = ev.isParam;



        if (ev.underLock) {
            acc.hasLocked = true;
            acc.lockedEvents.push_back(&ev);
        } else {
            acc.hasUnlocked = true;
            acc.unlockedEvents.push_back(&ev);
        }
    }

    // 输出结果
    log("===== 每个线程内变量加锁/未加锁访问统计 =====");

    for (auto it = perThreadVar.begin(); it != perThreadVar.end(); ++it) {
        const QString key = it.key();
        const ThreadVarAccess &acc = it.value();

        const QString thread  = key.section("|", 0, 0);

        QString scope;
        if (acc.isParam)       scope = "参数变量";
        else if (acc.isLocal)  scope = "局部变量";
        else if (acc.isGlobal) scope = "全局变量";
        else                   scope = "未知作用域";

        QString result;
        if (acc.hasLocked && !acc.hasUnlocked)      result = "仅加锁访问";
        else if (!acc.hasLocked && acc.hasUnlocked) result = "未加锁访问";
        else                                        result = "加锁 + 未加锁混合访问";

        log(QString("线程 %1, 变量 %2 [%3], %4 (symbolId=%5): %6")
                .arg(thread)
                .arg(acc.varName)
                .arg(acc.varType)
                .arg(scope)
                .arg(acc.symbolId)
                .arg(result));

        if(!acc.lockedEvents.isEmpty()){
            log("  加锁访问:");
            for(auto *ev : acc.lockedEvents){
                log(QString("    [%1:%2] %3")
                        .arg(ev->funcName)
                        .arg(ev->line)
                        .arg(ev->code));
            }
        }
        if(!acc.unlockedEvents.isEmpty()){
            log("  未加锁访问:");
            for(auto *ev : acc.unlockedEvents){
                log(QString("    [%1:%2] %3")
                        .arg(ev->funcName)
                        .arg(ev->line)
                        .arg(ev->code));
            }
        }
    }
}

void MainWindow::logUninitPtrDerefSummary(const analysis::AnalysisOutput &out)
{
    log(tr("===== 未初始化指针解引用检查 ====="));

    for (const analysis::VarEvent &ev : out.events) {
        // 只关心指针解引用，且被分析标记为“可能未初始化”
        if (ev.action != QStringLiteral("PtrDeref"))
            continue;
        if (!ev.isUninitPtrDeref)
            continue;

        // 作用域描述：参数 / 局部 / 全局
        QString scope;
        if (ev.isParam)       scope = tr("参数变量");
        else if (ev.isLocal)  scope = tr("局部变量");
        else if (ev.isGlobal) scope = tr("全局变量");
        else                  scope = tr("未知作用域");

        // 带上线程前缀（如果有线程信息）
        QString threadPrefix;
        if (!ev.threadVarName.isEmpty()) {
            threadPrefix = tr("线程 %1(%2, entry=%3), ")
                               .arg(ev.threadVarName,
                                    ev.threadApi.isEmpty()
                                        ? QStringLiteral("?")
                                        : ev.threadApi,
                                    ev.threadEntryName.isEmpty()
                                        ? ev.funcName
                                        : ev.threadEntryName);
        }

        const QString varName =
            ev.varName.isEmpty() ? QStringLiteral("<匿名>") : ev.varName;

        const QString msg =
            tr("【未初始化指针解引用】%1变量 %2 [%3], %4 (symbolId=%5) 在函数 %6, B%7, 行 %8\n    语句: %9")
                .arg(threadPrefix)
                .arg(varName)
                .arg(ev.varType)
                .arg(scope)
                .arg(ev.symbolId)
                .arg(ev.funcName)
                .arg(ev.blockId)
                .arg(ev.line)
                .arg(ev.code);

        log(msg);
    }
}

void MainWindow::on_startAnalyze_Button_clicked()
{
    if (m_projectRoot.isEmpty()) {
        log(tr("项目路径为空！"));
        return;
    }

    // 收集项目中的所有源文件
    QList<QString> sourceFiles;
    const QModelIndex parentIndex = m_model->index(m_projectRoot);
    io::collectFiles(m_model, parentIndex, sourceFiles);

    if (sourceFiles.isEmpty()) {
        log(tr("无源文件！"));
        return;
    }
    log(tr("共%1个文件").arg(sourceFiles.size()));

    const QString outRoot = QDir(m_projectRoot).filePath(QStringLiteral("qtAnalysis"));
    QDir().mkpath(outRoot);

    int index = 0;
    for (const QString &file : sourceFiles) {
        ++index;
        const QString rel = QDir(m_projectRoot).relativeFilePath(file);
        log(tr("[%1/%2] 处理 %3")
                .arg(index)
                .arg(sourceFiles.size())
                .arg(rel));

        // 各种 clang 输出路径
        const QString astOut =
            QDir(outRoot).filePath(rel + QStringLiteral(".ast.json"));
        const QString cfgOut =
            QDir(outRoot).filePath(rel + QStringLiteral(".cfg.txt"));
        const QString symOut =
            QDir(outRoot).filePath(rel + QStringLiteral(".symbols.txt"));
        const QString analysisOut =
            QDir(outRoot).filePath(rel + QStringLiteral(".analysis.json"));

        // 确保输出目录存在
        QDir().mkpath(QFileInfo(astOut).path());
        QDir().mkpath(QFileInfo(cfgOut).path());
        QDir().mkpath(QFileInfo(symOut).path());
        QDir().mkpath(QFileInfo(analysisOut).path());

        // 调用外部工具生成各种结果
        io::runSymbolsNm(file, symOut);
        io::runClangPluginAnalysis(file, analysisOut);

        // Qt 端：读取 analysis.json -> 做静态分析 -> 输出结果
        analysis::ProgramModel model;
        if (!analysis::loadProgramModelFromFile(analysisOut, model)) {
            log(tr("加载 analysis.json 失败: %1").arg(analysisOut));
            continue;
        }
        logThreadSummary(this, model, file);
        const analysis::AnalysisOutput result = analysis::analyzeProgram(model);
        logAnalysisSummary(this, result, file);
        logThreadVarLockSummary(result);
        logUninitPtrDerefSummary(result);
        logAnalysisEvents(this, result, file);
    }

    log(tr("分析完成！"));
}

void MainWindow::on_chooseProject_Button_clicked()
{
    const QString dir = QFileDialog::getExistingDirectory(
        this,
        tr("选择文件根目录"),
        QDir::homePath(),
        QFileDialog::ShowDirsOnly);

    if (!dir.isEmpty()) {
        m_projectRoot = dir;
        ui->pathBrowser->setText(dir);

        m_model->setRootPath(dir);
        ui->fileBrowser->setModel(m_model);
        ui->fileBrowser->setRootIndex(m_model->index(dir));
    }
}
