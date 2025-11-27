#include "Toolchain.h"
#include <QProcess>
#include <QDir>
#include <QStringList>
#include <QDebug>
#include <QJsonParseError>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonArray>

namespace io{
QStringList getQtCflags()
{
    static QStringList cached;
    if (!cached.isEmpty()) return cached;
    QProcess pkg;
    pkg.start("pkg-config", {"--cflags", "Qt5Widgets"});
    pkg.waitForFinished();

    QString out = QString::fromUtf8(pkg.readAllStandardOutput()).trimmed();
    QStringList parts = out.split(' ', Qt::SkipEmptyParts);

    for (const QString &arg : parts) {
        cached << arg;
    }
    return cached;
}

bool runSymbolsNm(const QString &file, const QString &outFile)
{
    QString tmpObj = outFile + ".o";
    // 编译成临时对象
    QProcess p;
    QStringList args;
    args<<"-std=c++17";
    args<<getQtCflags();

#ifdef Q_OS_LINUX
    args<<"-pthread";
#endif

    args<<"-fno-implicit-templates";
    args<<"-fno-color-diagnostics";
    args<<"-c";
    args<<file;
    args<<"-o";
    args<<tmpObj;
    p.start("clang", args);
    bool ok = p.waitForFinished(-1);
    if(!ok)
    {
        qWarning()<<"clang进程没有正常结束："<<p.errorString();
        return false;
    }
    if (p.exitStatus() != QProcess::NormalExit || p.exitCode() != 0)
    {
        qWarning()<<"编译对象文件失败："<<file;
        return false;
    }
    // 用 llvm-nm 提取符号
    QProcess nm;
    nm.start("llvm-nm", {"-g", "--defined-only", tmpObj});
    nm.waitForFinished(-1);
    QByteArray out = nm.readAllStandardOutput();

    QFile f(outFile);
    if (f.open(QIODevice::WriteOnly))
    {
        f.write(out);
        f.close();
    }
    QFile::remove(tmpObj);
    return true;
}

void runClangPluginAnalysis(const QString &file, const QString &outJson) {
    QStringList args;
    args << "-std=c++17"
         << "-fsyntax-only"
         << "-fno-implicit-templates"
         << "-fno-color-diagnostics"
         << getQtCflags()
         << "-Xclang" << "-load"
         << "-Xclang" << "/home/brunost/ThreadAnalyzer/clang_cfg_plugin/build/CfgMapPlugin.so"
         << "-Xclang" << "-plugin"
         << "-Xclang" << "cfg-map-plugin"    // ★ 确保这里和插件注册名一致
         << file;

    QProcess p;
    p.start("clang++", args);
    if (!p.waitForFinished(-1)) {
        qWarning() << "clang++ 插件超时:" << p.errorString();
        return;
    }

    QByteArray out = p.readAllStandardOutput();
    QByteArray err = p.readAllStandardError();

    qDebug() << "clang++ exitCode =" << p.exitCode()
             << "exitStatus =" << p.exitStatus();
    qDebug() << "stderr =" << err;

    if (p.exitStatus() != QProcess::NormalExit || p.exitCode() != 0) {
        return;
    }

    QFile f(outJson);
    if (f.open(QIODevice::WriteOnly | QIODevice::Truncate)) {
        f.write(out);
        f.close();
    }
}


}

