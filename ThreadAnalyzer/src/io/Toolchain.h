#ifndef TOOLCHAIN_H
#define TOOLCHAIN_H

#include <QString>

namespace io {
bool runClangAst(const QString& file, const QString& outfile);
bool runClangCfg(const QString& file, const QString& outfile);
bool runSymbolsNm(const QString& file, const QString& outfile);
void runClangPluginAnalysis(const QString &file, const QString &outJson);
QStringList getQtCflags();
}

#endif // TOOLCHAIN_H
