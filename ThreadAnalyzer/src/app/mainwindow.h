// mainwindow.h

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QFileSystemModel>

QT_BEGIN_NAMESPACE
namespace Ui { class MainWindow; }
QT_END_NAMESPACE

// 只做一个前向声明，避免头文件互相 include 过多
namespace analysis {
struct AnalysisOutput;
struct ProgramModel;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = nullptr);
    ~MainWindow();

private slots:
    void on_startAnalyze_Button_clicked();
    void on_chooseProject_Button_clicked();

private:
    void log(const QString& msg);

    // 专门负责把分析结果输出到 log
    void logAnalysisSummary(MainWindow *self,
                            const analysis::AnalysisOutput &out,
                            const QString &sourceFile);
    void logAnalysisEvents(MainWindow *self,
                           const analysis::AnalysisOutput &out,
                           const QString &sourceFile);
    void logThreadSummary(MainWindow *self,
                          const analysis::ProgramModel &model,
                          const QString &sourceFile);
    void logThreadVarLockSummary(const analysis::AnalysisOutput &out);
    void logUninitPtrDerefSummary(const analysis::AnalysisOutput &out);

    Ui::MainWindow *ui;
    QString m_projectRoot;
    QFileSystemModel *m_model = nullptr;
};

#endif // MAINWINDOW_H
