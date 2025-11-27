#include "util/Log.h"
#include <QDebug>
#include <atomic>

namespace util {

static std::atomic<LogFn*> gLogger{nullptr};

static void defaultSink(Level lv, const QString& s) {
    switch (lv) {
    case Level::Info:  qInfo().noquote()    << s; break;
    case Level::Warn:  qWarning().noquote() << s; break;
    case Level::Error: qCritical().noquote()<< s; break;
    }
}

void setLogger(LogFn fn) {
    auto* p = fn ? new LogFn(std::move(fn)) : nullptr;
    auto* old = gLogger.exchange(p);
    if (old) delete old;
}

static inline void sink(Level lv, const QString& s) {
    if (auto* p = gLogger.load()) { (*p)(lv, s); }
    else { defaultSink(lv, s); }
}

void log(const QString& msg)      { sink(Level::Info,  msg); }
void logWarn(const QString& msg)  { sink(Level::Warn,  msg); }
void logError(const QString& msg) { sink(Level::Error, msg); }

} // namespace util
