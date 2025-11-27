#include "clang/AST/AST.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/Analysis/CFG.h"
#include "clang/AST/Decl.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Lex/Lexer.h"
#include "clang/AST/DeclCXX.h"   // CXXRecordDecl / CXXMethodDecl 等
#include "clang/Index/USRGeneration.h"
#include "clang/AST/ASTTypeTraits.h"
#include "clang/AST/ParentMapContext.h"
#include "llvm/Support/JSON.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallString.h"

using namespace clang;

namespace {

/// 把 SourceLocation 转成 "file:line:col" 字符串。
static void fillLocation(const SourceManager &SM, SourceLocation Loc,
                         llvm::json::Object &Obj) {
    PresumedLoc PL = SM.getPresumedLoc(Loc);
    if (!PL.isValid())
        return;
    Obj["file"] = std::string(PL.getFilename());
    Obj["line"] = static_cast<int>(PL.getLine());
    Obj["col"]  = static_cast<int>(PL.getColumn());
}

/// 提取一个语句的大致源码文本（用于调试/展示）
static std::string getSourceForStmt(const Stmt *S, const ASTContext &Ctx) {
    if (!S) return "";
    const SourceManager &SM = Ctx.getSourceManager();
    SourceRange SR = S->getSourceRange();
    if (SR.isInvalid()) return "";

    CharSourceRange CR = CharSourceRange::getTokenRange(SR);
    bool Invalid = false;
    llvm::StringRef Text =
        Lexer::getSourceText(CR, SM, Ctx.getLangOpts(), &Invalid);
    if (Invalid) return "";
    return Text.str();
}

static std::string getUSRForDecl(const Decl *D) {
    if (!D) return "";

    llvm::SmallString<128> Buf;
    // 返回值为 0 表示成功
    if (clang::index::generateUSRForDecl(D, Buf))
        return "";  // 生成失败就返回空串

    return Buf.str().str();  // 转成 std::string
}

static bool isStdThreadType(QualType QT) {
    const auto *RT = QT->getAs<RecordType>();
    if (!RT)
        return false;

    const auto *RD = llvm::dyn_cast<CXXRecordDecl>(RT->getDecl());
    if (!RD)
        return false;

    // 类名必须是标识符，且为 "thread"
    const IdentifierInfo *ClassId = RD->getIdentifier();
    if (!ClassId || !ClassId->isStr("thread"))
        return false;

    // 检查是否在命名空间 std 里
    const DeclContext *DC = RD->getDeclContext();
    const auto *NS = llvm::dyn_cast_or_null<NamespaceDecl>(DC);
    if (!NS)
        return false;

    const IdentifierInfo *NSId = NS->getIdentifier();
    if (!NSId || !NSId->isStr("std"))
        return false;

    return true;
}


class CfgMapConsumer : public ASTConsumer {
public:
    explicit CfgMapConsumer(ASTContext *ctx) : Ctx(ctx) {}

    void HandleTranslationUnit(ASTContext &Context) override {
        const SourceManager &SM = Context.getSourceManager();
        const TranslationUnitDecl *TU = Context.getTranslationUnitDecl();

        llvm::json::Object root;

        // === 新增：TU 级别信息 ===
        const FileEntry *MainFile = SM.getFileEntryForID(SM.getMainFileID());
        if (MainFile) {
            root["tuMainFile"] = MainFile->getName().str();
        }

        // 如果你想弄个更短的 tuId（可选）
        // 这里简单用文件名做 tuId，或者你也可以做一个 hash
        if (MainFile) {
            root["tuId"] = MainFile->getName().str();
        }

        // 1) 收集全局变量
        llvm::json::Array globalVars;
        for (const Decl *D : TU->decls()) {
            if (const auto *VD = llvm::dyn_cast<VarDecl>(D)) {
                // 定义在函数/方法之外的变量视为全局
                if (VD->isDefinedOutsideFunctionOrMethod()) {
                    llvm::json::Object varObj;
                    unsigned id = getVarId(VD);
                    varObj["id"]   = static_cast<int>(id);
                    varObj["name"] = VD->getNameAsString();
                    varObj["type"] = VD->getType().getAsString();
                    varObj["astId"] = llvm::formatv("{0:x}",
                                                    reinterpret_cast<uintptr_t>(VD)).str();
                    std::string usr = getUSRForDecl(VD);
                    if (!usr.empty())
                        varObj["symbolId"] = usr;    // 新字段：跨 TU 稳定的 ID

                    fillLocation(SM, VD->getLocation(), varObj);
                    globalVars.push_back(std::move(varObj));
                }
            }
        }
        root["globalVars"] = std::move(globalVars);

        // 1.5) 收集 struct / class 类型信息
        llvm::json::Array recordTypes;
        collectRecordTypes(Context, recordTypes);
        root["recordTypes"] = std::move(recordTypes);

        // 2) 收集函数 + CFG
        llvm::json::Array functions;
        for (const Decl *D : TU->decls()) {
            SourceLocation Loc = D->getLocation();
            if(SM.isInSystemHeader(Loc) || SM.isInSystemMacro(Loc)) continue;

            const auto *FD = llvm::dyn_cast<FunctionDecl>(D);
            if (!FD || !FD->hasBody())
                continue;

            const FunctionDecl *Def = nullptr;
            if(!FD->hasBody(Def))continue;
            if(FD!=Def)continue;
            FD = Def;

            // buildCFG 需要 Stmt*
            const Stmt *BodyConst = FD->getBody();
            Stmt *Body = const_cast<Stmt *>(BodyConst);

            CFG::BuildOptions BO;
            std::unique_ptr<CFG> cfg = CFG::buildCFG(FD, Body, &Context, BO);
            if (!cfg)
                continue;

            llvm::json::Object funcObj;
            funcObj["name"]  = FD->getNameAsString();
            funcObj["astId"] = llvm::formatv("{0:x}",
                                             reinterpret_cast<uintptr_t>(FD)).str();
            funcObj["isMain"] = FD->isMain();
            fillLocation(SM, FD->getLocation(), funcObj);

            std::string fUsr = getUSRForDecl(FD);
            if(!fUsr.empty())funcObj["symbolId"] = fUsr;

            // 2.1 参数（也给 VarId）
            llvm::json::Array params;
            for (const auto *PVD : FD->parameters()) {
                llvm::json::Object paramObj;
                unsigned id = getVarId(PVD);
                paramObj["id"]   = static_cast<int>(id);
                paramObj["name"] = PVD->getNameAsString();
                paramObj["type"] = PVD->getType().getAsString();
                paramObj["astId"] = llvm::formatv("{0:x}",
                                                  reinterpret_cast<uintptr_t>(PVD)).str();
                std::string usr = getUSRForDecl(PVD);
                if (!usr.empty())
                    paramObj["symbolId"] = usr;    // 新字段：跨 TU 稳定的 ID
                fillLocation(SM, PVD->getLocation(), paramObj);
                params.push_back(std::move(paramObj));
            }
            funcObj["params"] = std::move(params);

            // 2.2 局部变量
            llvm::json::Array localVars;
            collectLocalVariables(FD, FD->getBody(), Context, localVars);
            funcObj["localVars"] = std::move(localVars);

            // 2.3 CFG blocks
            llvm::json::Array blocks;

            for (const CFGBlock *B : *cfg) {
                if (!B) continue;

                llvm::json::Object blockObj;
                blockObj["id"] = static_cast<int>(B->getBlockID());

                // 前驱
                llvm::json::Array predecessors;
                for (CFGBlock::const_pred_iterator PredIt = B->pred_begin();
                     PredIt != B->pred_end(); ++PredIt) {
                    if (const CFGBlock *Pred = *PredIt)
                        predecessors.push_back(static_cast<int>(Pred->getBlockID()));
                }
                blockObj["predecessors"] = std::move(predecessors);

                // 后继
                llvm::json::Array successors;
                for (CFGBlock::const_succ_iterator SuccIt = B->succ_begin();
                     SuccIt != B->succ_end(); ++SuccIt) {
                    if (const CFGBlock *Succ = *SuccIt)
                        successors.push_back(static_cast<int>(Succ->getBlockID()));
                }
                blockObj["successors"] = std::move(successors);

                // 终结语句 / 条件信息（用于分支/循环分析）
                addTerminatorInfo(*B, Context, blockObj);

                // 该块内使用到的变量（简单集合）
                llvm::json::Array usedVars;
                collectUsedVariablesInBlock(B, Context, usedVars);
                blockObj["usedVars"] = std::move(usedVars);

                // block 内语句
                llvm::json::Array stmts;
                unsigned stmtIndex = 0;

                for (const CFGElement &E : *B) {
                    if (auto CS = E.getAs<CFGStmt>()) {
                        const Stmt *S = CS->getStmt();
                        uintptr_t astPtr = reinterpret_cast<uintptr_t>(S);

                        llvm::json::Object stmtObj;
                        stmtObj["index"] = static_cast<int>(stmtIndex);
                        stmtObj["astId"] =
                            llvm::formatv("{0:x}",
                                          static_cast<uint64_t>(astPtr)).str();
                        stmtObj["kind"]  = S->getStmtClassName();
                        stmtObj["code"]  = getSourceForStmt(S, Context);
                        fillLocation(SM, S->getBeginLoc(), stmtObj);

                        // 分析此语句中的“指针初始化 / 赋值”信息
                        collectPointerInfoForStmt(S, Context, stmtObj);
                        // 线程创建+线程入口函数
                        collectThreadInfoForStmt(S, Context, stmtObj);
                        stmtObj["tree"] = buildStmtTree(S, Context);

                        stmts.push_back(std::move(stmtObj));
                        ++stmtIndex;
                    }
                }

                blockObj["stmts"] = std::move(stmts);
                blocks.push_back(std::move(blockObj));
            }

            funcObj["blocks"] = std::move(blocks);
            functions.push_back(std::move(funcObj));
        }

        root["functions"] = std::move(functions);

        // pretty-print JSON
        llvm::outs() << llvm::formatv("{0:2}\n", llvm::json::Value(std::move(root)));
    }

private:
    ASTContext *Ctx;

    llvm::json::Object buildStmtTree(const Stmt *S, const ASTContext &Context) {
        llvm::json::Object node;
        if (!S) return node;

        const SourceManager &SM = Context.getSourceManager();

        // ======== 特殊处理：带初始化的单一 VarDecl 的 DeclStmt ========
        if (const auto *DS = llvm::dyn_cast<DeclStmt>(S)) {
            const VarDecl *VD = nullptr;

            // 只在 "int *p = g_Global;" 这种：DeclStmt 里恰好一个 VarDecl 时触发
            for (const Decl *D : DS->decls()) {
                if (const auto *CurVD = llvm::dyn_cast<VarDecl>(D)) {
                    if (!VD)
                        VD = CurVD;
                    else {
                        // 多个 VarDecl 就算了，走默认逻辑
                        VD = nullptr;
                        break;
                    }
                }
            }

            if (VD && VD->getInit()) {
                // 构造一个“赋值根节点”
                node["kind"]  = "InitAssign"; // 自定义 kind，表示声明初始化
                node["astId"] = llvm::formatv(
                                    "{0:x}", reinterpret_cast<uintptr_t>(S)).str();
                node["code"]  = getSourceForStmt(S, Context);
                node["op"]    = "=";
                fillLocation(SM, S->getBeginLoc(), node);

                llvm::json::Array children;

                // --- 左子树：int *p ---
                llvm::json::Object lhs;
                lhs["kind"]  = "VarLHS";
                lhs["astId"] = llvm::formatv(
                                   "{0:x}", reinterpret_cast<uintptr_t>(VD)).str();
                lhs["name"]  = VD->getNameAsString();
                lhs["type"]  = VD->getType().getAsString();

                unsigned id = getVarId(VD);
                lhs["varId"]   = static_cast<int>(id);
                lhs["varName"] = VD->getNameAsString();
                lhs["varType"] = VD->getType().getAsString();
                std::string usr = getUSRForDecl(VD);
                if (!usr.empty())
                    lhs["symbolId"] = usr;    // 新字段：跨 TU 稳定的 ID

                fillLocation(SM, VD->getLocation(), lhs);

                // 如果你愿意，这里可以进一步把 "int *p" 拆成子树
                // 比如 kind="PointerType"，children=[ "int", "p" ] 之类，
                // 先省略，保持 VarLHS 自己是一个叶子节点即可。

                children.push_back(std::move(lhs));

                // --- 右子树：初始化表达式 (g_Global / 其他任意 Expr) ---
                const Expr *Init = VD->getInit()->IgnoreParenImpCasts();
                children.push_back(buildStmtTree(Init, Context));

                node["children"] = std::move(children);
                return node; // 特殊路径到此结束
            }
        }

        // 基本信息：种类、地址、源码
        node["kind"]  = S->getStmtClassName();
        node["astId"] = llvm::formatv("{0:x}",
                                      reinterpret_cast<uintptr_t>(S)).str();
        node["code"]  = getSourceForStmt(S, Context);
        fillLocation(SM, S->getBeginLoc(), node);

        // ★ 如果是二元运算，补上具体的操作符 '='、'+'
        if (const auto *BO = llvm::dyn_cast<BinaryOperator>(S)) {
            node["op"] = BinaryOperator::getOpcodeStr(BO->getOpcode()).str();
        }
        if (const auto *CAO = llvm::dyn_cast<CompoundAssignOperator>(S)) {
            node["op"] = BinaryOperator::getOpcodeStr(CAO->getOpcode()).str();
        }
        if (const auto *UO = llvm::dyn_cast<UnaryOperator>(S)) {
            node["op"] = UnaryOperator::getOpcodeStr(UO->getOpcode()).str(); // 比如 '*', '&', '++'
        }

        // 如果是变量引用，还可以挂上 VarId/VarDeclAstId/作用域信息，方便后续分析
        if (auto *DRE = llvm::dyn_cast<DeclRefExpr>(S)) {
            if (auto *VD = llvm::dyn_cast<VarDecl>(DRE->getDecl())) {
                unsigned id = getVarId(VD);

                node["varId"]   = static_cast<int>(id);
                node["varName"] = VD->getNameAsString();
                node["varType"] = VD->getType().getAsString();
                std::string usr = getUSRForDecl(VD);
                if (!usr.empty())
                    node["symbolId"] = usr;    // 新字段：跨 TU 稳定的 ID

                // 1) 这个引用对应的 VarDecl 的 astId
                node["varDeclAstId"] = llvm::formatv(
                                           "{0:x}", reinterpret_cast<uintptr_t>(VD)).str();

                // 2) 一些作用域/存储类别标记（可选，但很好用）
                bool isParam  = llvm::isa<ParmVarDecl>(VD);
                bool isLocal  = VD->isLocalVarDecl() && !VD->isStaticLocal();
                bool isGlobal = VD->isDefinedOutsideFunctionOrMethod();
                // 或者用 VD->hasGlobalStorage() 看你偏好哪个定义

                node["isParam"]  = isParam;
                node["isLocal"]  = isLocal;
                node["isGlobal"] = isGlobal;
            }
        }

        // 如果是成员函数调用，比如 g_m.lock()
        if (const auto *MCE = llvm::dyn_cast<CXXMemberCallExpr>(S)) {
            if (const CXXMethodDecl *MD = MCE->getMethodDecl()) {
                node["memberName"] = MD->getNameAsString();  // "lock" / "unlock" 等
            }
        }

        // 如果是成员表达式，比如 g_m.lock / obj.field
        if (const auto *ME = llvm::dyn_cast<MemberExpr>(S)) {
            if (const ValueDecl *MD = ME->getMemberDecl()) {
                node["memberName"] = MD->getNameAsString();  // "lock" / "unlock" / "fieldName"
            }
        }

        if (auto *CE = dyn_cast<CallExpr>(S)) {
            const Expr *Callee = CE->getCallee()->IgnoreParenImpCasts();
            if (auto *DRE = dyn_cast<DeclRefExpr>(Callee)) {
                if (auto *FD = dyn_cast<FunctionDecl>(DRE->getDecl())) {
                    node["calleeName"] = FD->getNameAsString();
                    std::string fUsr = getUSRForDecl(FD);
                    if (!fUsr.empty())
                        node["calleeSymbolId"] = fUsr;
                }
            }
        }

        // 递归子节点
        llvm::json::Array children;
        for (const Stmt *Child : S->children()) {
            if (!Child) continue;
            children.push_back(buildStmtTree(Child, Context));
        }
        if (!children.empty())
            node["children"] = std::move(children);

        return node;
    }

    // ==== 变量 ID 管理 ====
    llvm::DenseMap<const VarDecl *, unsigned> VarIdMap;
    unsigned NextVarId = 0;

    unsigned getVarId(const VarDecl *VD) {
        auto It = VarIdMap.find(VD);
        if (It != VarIdMap.end())
            return It->second;
        unsigned id = NextVarId++;
        VarIdMap[VD] = id;
        return id;
    }

    // ==== 局部变量收集 ====
    void collectLocalVariables(const FunctionDecl *FD, const Stmt *Body,
                               ASTContext &Context, llvm::json::Array &vars) {
        const SourceManager &SM = Context.getSourceManager();

        std::function<void(const Stmt *)> traverse = [&](const Stmt *S) {
            if (!S) return;

            if (const auto *DS = llvm::dyn_cast<DeclStmt>(S)) {
                for (const Decl *D : DS->decls()) {
                    if (const auto *VD = llvm::dyn_cast<VarDecl>(D)) {
                        // 排除参数 & 全局；这里只收真正的局部变量
                        if (VD->isLocalVarDecl() && !VD->isStaticLocal()) {
                            llvm::json::Object varObj;
                            unsigned id = getVarId(VD);
                            varObj["id"]   = static_cast<int>(id);
                            varObj["name"] = VD->getNameAsString();
                            varObj["type"] = VD->getType().getAsString();
                            varObj["astId"] = llvm::formatv("{0:x}",
                                                            reinterpret_cast<uintptr_t>(VD)).str();
                            std::string usr = getUSRForDecl(VD);
                            if (!usr.empty())
                                varObj["symbolId"] = usr;    // 新字段：跨 TU 稳定的 ID

                            fillLocation(SM, VD->getLocation(), varObj);
                            vars.push_back(std::move(varObj));
                        }
                    }
                }
            }

            for (const Stmt *Child : S->children())
                traverse(Child);
        };

        traverse(Body);
    }

    // ==== 结构体 / 类 类型信息 ====
    void collectRecordTypes(ASTContext &Context, llvm::json::Array &records) {
        const SourceManager &SM = Context.getSourceManager();
        const TranslationUnitDecl *TU = Context.getTranslationUnitDecl();

        // 防止同一个类型重复（有多个声明），只保留 definition
        llvm::SmallPtrSet<const RecordDecl *, 32> Seen;

        for (const Decl *D : TU->decls()) {
            const auto *RD = llvm::dyn_cast<RecordDecl>(D);
            if (!RD) continue;
            if (RD->isImplicit()) continue;  // 忽略编译器生成的

            // 只要有 definition（跳过纯 forward decl）
            const RecordDecl *Def = RD->getDefinition();
            if (!Def) continue;

            RD = Def;
            if (!Seen.insert(RD).second)
                continue; // 已处理过该定义

            if (SM.isInSystemHeader(RD->getLocation()) ||
                SM.isInSystemMacro(RD->getLocation()))
                continue; // 忽略系统头

            llvm::json::Object recObj;
            recObj["astId"] = llvm::formatv(
                                  "{0:x}", reinterpret_cast<uintptr_t>(RD)).str();
            recObj["name"] = RD->getNameAsString();        // 结构体/类名，可能为空(匿名)
            recObj["tagKind"] = RD->getKindName();         // "struct" / "class" / "union"
            recObj["isStruct"] = RD->isStruct();
            recObj["isClass"]  = RD->isClass();
            recObj["isUnion"]  = RD->isUnion();

            fillLocation(SM, RD->getLocation(), recObj);

            // 字段列表
            llvm::json::Array fields;
            for (const FieldDecl *FD : RD->fields()) {
                llvm::json::Object fieldObj;
                fieldObj["astId"] = llvm::formatv(
                                        "{0:x}", reinterpret_cast<uintptr_t>(FD)).str();
                fieldObj["name"] = FD->getNameAsString();
                fieldObj["type"] = FD->getType().getAsString();
                fillLocation(SM, FD->getLocation(), fieldObj);
                fields.push_back(std::move(fieldObj));
            }
            if (!fields.empty())
                recObj["fields"] = std::move(fields);

            // 如果是 C++ 的 class/struct，再把显式成员函数也导出来（可选）
            if (const auto *CRD = llvm::dyn_cast<CXXRecordDecl>(RD)) {
                llvm::json::Array methods;
                for (const CXXMethodDecl *MD : CRD->methods()) {
                    if (MD->isImplicit()) continue;  // 忽略编译器隐式生成的（默认构造、拷贝构造等）

                    llvm::json::Object mObj;
                    mObj["astId"] = llvm::formatv(
                                        "{0:x}", reinterpret_cast<uintptr_t>(MD)).str();
                    mObj["name"] = MD->getNameAsString();
                    mObj["resultType"] = MD->getReturnType().getAsString();
                    mObj["isStatic"] = MD->isStatic();
                    mObj["isConst"]  = MD->isConst();

                    // 可选：标记是不是构造 / 析构函数
                    mObj["isCtor"] = llvm::isa<CXXConstructorDecl>(MD);
                    mObj["isDtor"] = llvm::isa<CXXDestructorDecl>(MD);

                    fillLocation(SM, MD->getLocation(), mObj);

                    // === 新增：参数列表 ===
                    llvm::json::Array params;
                    for (const ParmVarDecl *P : MD->parameters()) {
                        llvm::json::Object pObj;
                        pObj["name"] = P->getNameAsString();
                        pObj["type"] = P->getType().getAsString();
                        pObj["astId"] = llvm::formatv(
                                            "{0:x}", reinterpret_cast<uintptr_t>(P)).str();
                        fillLocation(SM, P->getLocation(), pObj);
                        params.push_back(std::move(pObj));
                    }
                    if (!params.empty())
                        mObj["params"] = std::move(params);

                    methods.push_back(std::move(mObj));
                }
                if (!methods.empty())
                    recObj["methods"] = std::move(methods);
            }


            records.push_back(std::move(recObj));
        }
    }

    // ==== 块内使用变量集合（block 级别） ====
    void collectUsedVariablesInBlock(const CFGBlock *B, ASTContext &Context,
                                     llvm::json::Array &usedVars) {
        for (const CFGElement &E : *B) {
            if (auto CS = E.getAs<CFGStmt>()) {
                const Stmt *S = CS->getStmt();
                collectVariablesInStmt(S, Context, usedVars);
            }
        }
    }

    void collectVariablesInStmt(const Stmt *S, ASTContext &Context,
                                llvm::json::Array &usedVars) {
        if (!S) return;

        const SourceManager &SM = Context.getSourceManager();

        if (const auto *DRE = llvm::dyn_cast<DeclRefExpr>(S)) {
            if (const auto *VarD = llvm::dyn_cast<VarDecl>(DRE->getDecl())) {

                // 查重（以 astId 为 key）
                std::string thisAstId =
                    llvm::formatv("{0:x}", reinterpret_cast<uintptr_t>(VarD)).str();
                bool exists = false;
                for (const auto &var : usedVars) {
                    if (const auto *obj = var.getAsObject()) {
                        auto astIdIt = obj->find("astId");
                        if (astIdIt != obj->end() &&
                            astIdIt->second.getAsString().value_or("") == thisAstId) {
                            exists = true;
                            break;
                        }
                    }
                }

                if (!exists) {
                    llvm::json::Object varObj;
                    unsigned id = getVarId(VarD);
                    varObj["id"]   = static_cast<int>(id);
                    varObj["name"] = VarD->getNameAsString();
                    varObj["type"] = VarD->getType().getAsString();
                    varObj["astId"] = thisAstId;
                    varObj["isUsed"] = true;
                    fillLocation(SM, DRE->getLocation(), varObj);
                    usedVars.push_back(std::move(varObj));
                }
            }
        }

        for (const Stmt *Child : S->children())
            collectVariablesInStmt(Child, Context, usedVars);
    }

    // ==== 块终结语句 / 条件（用于 if/while/for/do 等控制流分析） ====
    void addTerminatorInfo(const CFGBlock &B, ASTContext &Context,
                           llvm::json::Object &blockObj) {
        const SourceManager &SM = Context.getSourceManager();
        const Stmt *Term = B.getTerminatorStmt();
        if (!Term)
            return;

        llvm::json::Object termObj;
        termObj["astKind"] = Term->getStmtClassName();
        termObj["astId"]   = llvm::formatv("{0:x}",
                                         reinterpret_cast<uintptr_t>(Term)).str();
        termObj["code"]    = getSourceForStmt(Term, Context);
        fillLocation(SM, Term->getBeginLoc(), termObj);

        const Expr *Cond = nullptr;
        if (const auto *IfS = llvm::dyn_cast<IfStmt>(Term))
            Cond = IfS->getCond();
        else if (const auto *WS = llvm::dyn_cast<WhileStmt>(Term))
            Cond = WS->getCond();
        else if (const auto *FS = llvm::dyn_cast<ForStmt>(Term))
            Cond = FS->getCond();
        else if (const auto *DS = llvm::dyn_cast<DoStmt>(Term))
            Cond = DS->getCond();

        if (Cond) {
            const Expr *CondE = Cond->IgnoreParenImpCasts();
            termObj["condAstId"] =
                llvm::formatv("{0:x}", reinterpret_cast<uintptr_t>(CondE)).str();
            termObj["condKind"] = CondE->getStmtClassName();
            termObj["condCode"] = getSourceForStmt(CondE, Context);

            // 解析简单的“指针 vs null/0”条件，便于后期做路径敏感分析
            llvm::json::Array condFacts;
            addPointerConditionFacts(CondE, condFacts);
            if (!condFacts.empty())
                termObj["condFacts"] = std::move(condFacts);
        }

        blockObj["terminator"] = std::move(termObj);

        // 对典型二分支条件块，标出 true/false successor
        if (Cond && B.succ_size() == 2) {
            auto It = B.succ_begin();
            const CFGBlock *Succ0 = *It;      // OK: AdjacentBlock 隐式转指针
            ++It;

            const CFGBlock *Succ1 = nullptr;
            if (It != B.succ_end()) {
                Succ1 = *It;                  // 这里同样是 AdjacentBlock -> CFGBlock* 的隐式转换
            }

            if (Succ0 && Succ1) {
                // Clang 的惯例：Succ0 为条件为真分支，Succ1 为假分支
                blockObj["trueSucc"]  = static_cast<int>(Succ0->getBlockID());
                blockObj["falseSucc"] = static_cast<int>(Succ1->getBlockID());
            }
        }
    }

    void addPointerConditionFacts(const Expr *CondE, llvm::json::Array &facts) {
        CondE = CondE->IgnoreParenImpCasts();

        const auto *BO = llvm::dyn_cast<BinaryOperator>(CondE);
        if (!BO || !BO->isComparisonOp())
            return;

        const Expr *LHS = BO->getLHS()->IgnoreParenImpCasts();
        const Expr *RHS = BO->getRHS()->IgnoreParenImpCasts();

        const DeclRefExpr *PtrRef = nullptr;
        const Expr *Other = nullptr;

        if (const auto *DRE = llvm::dyn_cast<DeclRefExpr>(LHS)) {
            PtrRef = DRE;
            Other = RHS;
        } else if (const auto *DRE = llvm::dyn_cast<DeclRefExpr>(RHS)) {
            PtrRef = DRE;
            Other = LHS;
        }

        if (!PtrRef)
            return;

        const auto *PtrVD = llvm::dyn_cast<VarDecl>(PtrRef->getDecl());
        if (!PtrVD || !PtrVD->getType()->isPointerType())
            return;

        unsigned ptrId = getVarId(PtrVD);

        llvm::json::Object fact;
        fact["ptrVarId"] = static_cast<int>(ptrId);
        fact["ptrName"]  = PtrVD->getNameAsString();
        fact["ptrType"]  = PtrVD->getType().getAsString();

        // 判断比较操作符
        StringRef OpStr = BinaryOperator::getOpcodeStr(BO->getOpcode());
        fact["op"] = OpStr.str();

        // 右边是否是 nullptr / 0
        if (llvm::isa<CXXNullPtrLiteralExpr>(Other)) {
            fact["kind"] = "ptr-vs-null";
            facts.push_back(std::move(fact));
            return;
        }

        if (const auto *IL = llvm::dyn_cast<IntegerLiteral>(Other)) {
            if (IL->getValue() == 0) {
                fact["kind"] = "ptr-vs-zero";
                facts.push_back(std::move(fact));
                return;
            }
        }

        // 其他情况，可以以后再扩展
    }

    // ==== 指针初始化 / 赋值信息 ====

    /// 在一个语句上收集“指针初始化/赋值”信息，写进 stmtObj["pointerInits"]
    void collectPointerInfoForStmt(const Stmt *S, ASTContext &Context,
                                   llvm::json::Object &stmtObj) {
        llvm::json::Array facts;

        // 1) VarDecl 带初始化：比如  int *p = &x;  int *p = q;  int *p = nullptr;
        if (const auto *DS = llvm::dyn_cast<DeclStmt>(S)) {
            for (const Decl *D : DS->decls()) {
                if (const auto *VD = llvm::dyn_cast<VarDecl>(D)) {
                    if (VD->getType()->isPointerType()) {
                        if (const Expr *Init = VD->getInit()) {
                            addPointerInitFact(facts, VD, Init);
                        }
                    }
                }
            }
        }

        // 2) 赋值语句：p = ...;   p += ...;  这里只关心 = / 复合赋值等
        if (const auto *BO = llvm::dyn_cast<BinaryOperator>(S)) {
            if (BO->isAssignmentOp()) {
                const Expr *LHS = BO->getLHS()->IgnoreParenImpCasts();
                if (const auto *LHSRef = llvm::dyn_cast<DeclRefExpr>(LHS)) {
                    if (const auto *VD =
                        llvm::dyn_cast<VarDecl>(LHSRef->getDecl())) {
                        if (VD->getType()->isPointerType()) {
                            addPointerInitFact(facts, VD, BO->getRHS());
                        }
                    }
                }
            }
        }

        if (!facts.empty())
            stmtObj["pointerInits"] = std::move(facts);
    }

    /// 针对一个“指针变量 = 某个表达式”的情况，生成一条或多条 pointerInit fact
    void addPointerInitFact(llvm::json::Array &facts,
                            const VarDecl *PtrVar,
                            const Expr *InitExpr) {
        if (!InitExpr) return;

        unsigned ptrId = getVarId(PtrVar);
        const Expr *E = InitExpr->IgnoreParenImpCasts();

        llvm::json::Object factBase;
        factBase["ptrVarId"] = static_cast<int>(ptrId);
        factBase["ptrType"]  = PtrVar->getType().getAsString();
        factBase["ptrName"]  = PtrVar->getNameAsString();

        // case 1: p = &x;
        if (const auto *UO = llvm::dyn_cast<UnaryOperator>(E)) {
            if (UO->getOpcode() == UO_AddrOf) {
                const Expr *Sub = UO->getSubExpr()->IgnoreParenImpCasts();
                if (const auto *DRE = llvm::dyn_cast<DeclRefExpr>(Sub)) {
                    if (const auto *TargetVD =
                        llvm::dyn_cast<VarDecl>(DRE->getDecl())) {
                        llvm::json::Object fact = factBase;
                        fact["kind"] = "addrOf";
                        fact["targetVarId"] =
                            static_cast<int>(getVarId(TargetVD));
                        fact["targetName"]  = TargetVD->getNameAsString();
                        fact["targetType"]  = TargetVD->getType().getAsString();
                        facts.push_back(std::move(fact));
                        return;
                    }
                }
            }
        }

        // case 2: p = q;   （q 也是指针）
        if (const auto *DRE = llvm::dyn_cast<DeclRefExpr>(E)) {
            if (const auto *SrcVD =
                llvm::dyn_cast<VarDecl>(DRE->getDecl())) {
                if (SrcVD->getType()->isPointerType()) {
                    llvm::json::Object fact = factBase;
                    fact["kind"]      = "copy";
                    fact["srcVarId"]  = static_cast<int>(getVarId(SrcVD));
                    fact["srcName"]   = SrcVD->getNameAsString();
                    fact["srcType"]   = SrcVD->getType().getAsString();
                    facts.push_back(std::move(fact));
                    return;
                }
            }
        }

        // case 3: p = nullptr;
        if (llvm::isa<CXXNullPtrLiteralExpr>(E)) {
            llvm::json::Object fact = factBase;
            fact["kind"] = "null";
            facts.push_back(std::move(fact));
            return;
        }

        // case 4: p = 0;
        if (const auto *IL = llvm::dyn_cast<IntegerLiteral>(E)) {
            if (IL->getValue() == 0) {
                llvm::json::Object fact = factBase;
                fact["kind"] = "zero";
                facts.push_back(std::move(fact));
                return;
            }
        }

        // 其他更复杂的 RHS（函数调用、三目、算术等），先标记为 unknown
        llvm::json::Object fact = factBase;
        fact["kind"] = "unknown";
        facts.push_back(std::move(fact));
    }

    // ==== 线程创建 / 入口函数信息 ====
    void collectThreadInfoForStmt(const Stmt *S,
                                  ASTContext &Context,
                                  llvm::json::Object &stmtObj) {
        llvm::json::Array facts;

        // 1) 普通函数调用：pthread_create
        if (const auto *CE = llvm::dyn_cast<CallExpr>(S)) {
            const Expr *Callee = CE->getCallee();
            if (Callee)
                Callee = Callee->IgnoreParenImpCasts();

            if (const auto *DRE = llvm::dyn_cast_or_null<DeclRefExpr>(Callee)) {
                if (const auto *FD =
                    llvm::dyn_cast<FunctionDecl>(DRE->getDecl())) {
                    if (const IdentifierInfo *Id = FD->getIdentifier()) {
                        if (Id->isStr("pthread_create")) {
                            addPthreadCreateFact(facts, CE, FD);
                        }
                    }
                }
            }
        }

        // 1.5) 声明语句：std::thread tX(...);   —— 在 DeclStmt 上也挂一份 threadOps
        if (const auto *DS = llvm::dyn_cast<DeclStmt>(S)) {
            for (const Decl *D : DS->decls()) {
                const auto *VD = llvm::dyn_cast<VarDecl>(D);
                if (!VD)
                    continue;

                QualType QT = VD->getType();
                if (!isStdThreadType(QT))
                    continue;

                const Expr *Init = VD->getInit();
                if (!Init)
                    continue;

                // 初始化表达式一般就是那个 CXXConstructExpr
                const auto *Ctor =
                    llvm::dyn_cast<CXXConstructExpr>(Init->IgnoreParenImpCasts());
                if (!Ctor)
                    continue;

                const auto *RT = QT->getAs<RecordType>();
                const auto *RD = RT ? llvm::dyn_cast<CXXRecordDecl>(RT->getDecl())
                                    : nullptr;

                addStdThreadCtorFact(facts, Ctor, RD, Context);
            }
        }

        // 2) C++ std::thread 构造：std::thread t(func, args...);  （非 DeclStmt 场景）
        if (const auto *Ctor = llvm::dyn_cast<CXXConstructExpr>(S)) {
            QualType QT = Ctor->getType();
            if (isStdThreadType(QT)) {
                const auto *RT = QT->getAs<RecordType>();
                const auto *RD = RT ? llvm::dyn_cast<CXXRecordDecl>(RT->getDecl())
                                    : nullptr;
                addStdThreadCtorFact(facts, Ctor, RD, Context);
            }
        }

        if (!facts.empty()) {
            stmtObj["threadOps"] = std::move(facts);
        }
    }

    void addPthreadCreateFact(llvm::json::Array &facts,
                              const CallExpr *CE,
                              const FunctionDecl *FD) {
        llvm::json::Object fact;
        fact["api"]  = "pthread_create";
        fact["kind"] = "createThread";

        // 记录调用点本身的符号（可选）
        fact["calleeName"] = FD->getNameAsString();
        std::string apiUsr = getUSRForDecl(FD);
        if (!apiUsr.empty())
            fact["calleeSymbolId"] = apiUsr;

        // 约定 param[0] 是 pthread_t*，param[2] 是入口函数
        if (CE->getNumArgs() >= 3) {
            // 线程句柄参数（pthread_t *thread）
            const Expr *ThreadHandleExpr = CE->getArg(0)->IgnoreParenImpCasts();
            if (const auto *ThreadDRE = llvm::dyn_cast<DeclRefExpr>(ThreadHandleExpr)) {
                if (const auto *ThreadVD =
                    llvm::dyn_cast<VarDecl>(ThreadDRE->getDecl())) {
                    fact["threadVarName"] = ThreadVD->getNameAsString();
                    fact["threadVarType"] = ThreadVD->getType().getAsString();
                    fact["threadVarAstId"] =
                        llvm::formatv("{0:x}", reinterpret_cast<uintptr_t>(ThreadVD)).str();
                    fact["threadVarId"] = static_cast<int>(getVarId(ThreadVD));
                }
            }

            // start_routine 参数（第 3 个参数）
            const Expr *Start = CE->getArg(2)->IgnoreParenImpCasts();

            // 处理 &foo 这种形式
            if (const auto *UO = llvm::dyn_cast<UnaryOperator>(Start)) {
                if (UO->getOpcode() == UO_AddrOf) {
                    Start = UO->getSubExpr()->IgnoreParenImpCasts();
                }
            }

            if (const auto *StartDRE = llvm::dyn_cast<DeclRefExpr>(Start)) {
                if (const auto *StartFD =
                    llvm::dyn_cast<FunctionDecl>(StartDRE->getDecl())) {
                    fact["startFuncName"] =
                        StartFD->getNameAsString();
                    fact["startFuncAstId"] =
                        llvm::formatv("{0:x}", reinterpret_cast<uintptr_t>(StartFD)).str();

                    std::string usr = getUSRForDecl(StartFD);
                    if (!usr.empty())
                        fact["startFuncSymbolId"] = usr;
                }
            }
        }

        facts.push_back(std::move(fact));
    }

    void addStdThreadCtorFact(llvm::json::Array &facts,
                              const CXXConstructExpr *Ctor,
                              const CXXRecordDecl *RD,
                              ASTContext &Context) {
        llvm::json::Object fact;
        fact["api"]  = "std::thread";
        fact["kind"] = "createThread";

        if (RD) {
            fact["className"] = RD->getNameAsString();
            fact["classAstId"] =
                llvm::formatv("{0:x}", reinterpret_cast<uintptr_t>(RD)).str();
        }

        // 记录构造函数自己（可选）
        if (const auto *CtorDecl = Ctor->getConstructor()) {
            fact["ctorAstId"] =
                llvm::formatv("{0:x}", reinterpret_cast<uintptr_t>(CtorDecl)).str();
            std::string usr = getUSRForDecl(CtorDecl);
            if (!usr.empty())
                fact["ctorSymbolId"] = usr;
        }

        // === 1) 找到对应的 VarDecl： std::thread t1(...); 里的 t1 ===
        const VarDecl *ThreadVD = nullptr;

        // 从 Ctor 开始，一直往父结点爬，最多爬几层防止死循环
        const Stmt *Cur = Ctor;
        for (int depth = 0; Cur && depth < 5 && !ThreadVD; ++depth) {
            auto Parents = Context.getParents(*Cur);
            if (Parents.empty())
                break;

            const Stmt *NextCur = nullptr;

            for (const auto &P : Parents) {
                // 直接是 VarDecl（常见情况：VarDecl 的 init 是 CXXConstructExpr）
                if (const auto *VD = P.get<VarDecl>()) {
                    ThreadVD = VD;
                    break;
                }

                // 是 DeclStmt（比如 DeclStmt 里面包着 VarDecl）
                if (const auto *DS = P.get<DeclStmt>()) {
                    for (const Decl *D : DS->decls()) {
                        if (const auto *VD = llvm::dyn_cast<VarDecl>(D)) {
                            ThreadVD = VD;
                            break;
                        }
                    }
                    if (ThreadVD)
                        break;
                }

                // 继续往上爬：父节点如果还是个 Stmt，就下一轮继续
                if (const Stmt *ParentS = P.get<Stmt>()) {
                    NextCur = ParentS;
                }
            }

            Cur = ThreadVD ? nullptr : NextCur;
        }

        if (ThreadVD) {
            fact["threadVarName"] = ThreadVD->getNameAsString();
            fact["threadVarType"] = ThreadVD->getType().getAsString();
            fact["threadVarAstId"] =
                llvm::formatv("{0:x}", reinterpret_cast<uintptr_t>(ThreadVD)).str();
            fact["threadVarId"] = static_cast<int>(getVarId(ThreadVD));
        }

        // === 2) std::thread t( <callable>, args... ); 的第一个参数：入口函数 ===
        if (Ctor->getNumArgs() >= 1) {
            const Expr *Callable = Ctor->getArg(0)->IgnoreParenImpCasts();

            // &foo / foo
            if (const auto *UO = llvm::dyn_cast<UnaryOperator>(Callable)) {
                if (UO->getOpcode() == UO_AddrOf) {
                    Callable = UO->getSubExpr()->IgnoreParenImpCasts();
                }
            }

            if (const auto *DRE = llvm::dyn_cast<DeclRefExpr>(Callable)) {
                if (const auto *FD =
                    llvm::dyn_cast<FunctionDecl>(DRE->getDecl())) {
                    fact["startFuncName"] =
                        FD->getNameAsString();
                    fact["startFuncAstId"] =
                        llvm::formatv("{0:x}", reinterpret_cast<uintptr_t>(FD)).str();
                    std::string usr = getUSRForDecl(FD);
                    if (!usr.empty())
                        fact["startFuncSymbolId"] = usr;
                }
            }

            // TODO: lambda / 函数对象 可以在这里再扩展
        }

        facts.push_back(std::move(fact));
    }

};

class CfgMapAction : public PluginASTAction {
protected:
    std::unique_ptr<ASTConsumer>
    CreateASTConsumer(CompilerInstance &CI, llvm::StringRef) override {
        return std::make_unique<CfgMapConsumer>(&CI.getASTContext());
    }

    bool ParseArgs(const CompilerInstance &,
                   const std::vector<std::string> &) override {
        return true;
    }
};

} // namespace

static FrontendPluginRegistry::Add<CfgMapAction>
    X("cfg-map-plugin", "Dump CFG as JSON with pointer/branch info");
