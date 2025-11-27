#ifndef LOG_H
#define LOG_H

#pragma once
#include <QString>
#include <functional>

namespace util {

enum class Level { Info, Warn, Error };
using LogFn = std::function<void(Level, const QString&)>;

// 传入空函数可恢复为默认（输出到控制台）
void setLogger(LogFn fn);

void log(const QString& msg);      // Info
void logWarn(const QString& msg);  // Warn
void logError(const QString& msg); // Error

} // namespace util

#endif // LOG_H
