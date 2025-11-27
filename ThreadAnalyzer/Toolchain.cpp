#include "Toolchain.h"
#include "util/PruneAst.h"
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

bool runClangAst(const QString &file, const QString &outFile)
{
    QProcess p;
    QStringList args;

    args << "-std=c++17";
    args << getQtCflags();
    args << "-fno-implicit-templates";     // 抑制模板实例化膨胀
    args << "-Xclang" << "-ast-dump=json";
    args << "-fsyntax-only" << file;

    p.start("clang", args);

    if (!p.waitForFinished(-1)) {
        qWarning() << "ast 超时";
        return false;
    }

    const QByteArray raw = p.readAllStandardOutput(); // JSON
    if (raw.isEmpty()) {
        qWarning() << "未输出 AST:" << p.readAllStandardError();
        return false;
    }

    // 解析 JSON → 剪枝
    QJsonParseError err{};
    QJsonDocument doc = QJsonDocument::fromJson(raw, &err);
    if (err.error != QJsonParseError::NoError) {
        qWarning() << "AST JSON 解析失败:" << err.errorString();
        return false;
    }

    QJsonValue pruned = util::pruneAstNode(doc.isObject()
                                               ? QJsonValue(doc.object())
                                               : QJsonValue(doc.array()));
    QJsonDocument outDoc;
    if (pruned.isObject()) outDoc = QJsonDocument(pruned.toObject());
    else if (pruned.isArray()) outDoc = QJsonDocument(pruned.toArray());
    else outDoc = doc; // 不该发生，兜底

    // 紧凑格式写盘（比缩进格式小很多）
    QByteArray compact = outDoc.toJson(QJsonDocument::Compact);

    QFile f(outFile);
    if (!f.open(QIODevice::WriteOnly)) return false;
    f.write(compact);
    f.close();
    return true;
}

bool runClangCfg(const QString &file, const QString &outFile)
{
    QProcess p;
    QStringList args;
    args<<"-std=c++17";
    args<<getQtCflags();
    args<<"-fno-implicit-templates";
    args<<"-fno-color-diagnostics";
    args<<"-Xclang";
    args<<"-analyze";
    args<<"-Xclang";
    args<<"-analyzer-checker=debug.DumpCFG";
    args<<"-fsyntax-only";
    args<<file;
    p.start("clang", args);
    bool ok = p.waitForFinished(-1);
    if(!ok)
    {
        qWarning()<<"clang进程没有正常结束："<<p.errorString();
        return false;
    }
    QByteArray out = p.readAllStandardOutput() + p.readAllStandardError();

    QFile f(outFile);
    if (f.open(QIODevice::WriteOnly))
    {
        f.write(out);
        f.close();
        return true;
    }
    return false;
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

}

