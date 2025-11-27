# ---- 基本信息 ----
TEMPLATE = app
TARGET   = QtThreadAnalyzer

# Qt 模块
QT += core gui widgets

# 语言/编译选项
CONFIG += c++17 release warn_on
# 如果想打开更多警告（GCC/Clang）：
QMAKE_CXXFLAGS += -Wall -Wextra
# MSVC 也会自动匹配 /std:c++17

# 头文件包含根目录（子目录用相对路径包含）
INCLUDEPATH += $$PWD/src

# ---- 源码收集：递归抓取 src/ 下所有 .cpp/.h ----
# $$files(pattern, true) 的第二个参数 true 表示递归子目录
SOURCES += $$files($$PWD/src/*.cpp, true)
HEADERS += $$files($$PWD/src/*.h,   true)
# 如果使用 Qt Designer 的 UI 文件（可选）
FORMS += $$PWD/src/app/MainWindow.ui




