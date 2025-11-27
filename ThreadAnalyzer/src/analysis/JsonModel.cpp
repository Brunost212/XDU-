// analysis/JsonModel.cpp
#include "analysis/JsonModel.h"
#include "util/Log.h"

#include <QFile>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonArray>

namespace analysis {

static QSharedPointer<AstNode> parseAstNode(const QJsonObject &obj,
                                            ProgramModel &model)
{
    auto node = QSharedPointer<AstNode>::create();
    node->astId   = obj.value("astId").toString();
    node->kind    = obj.value("kind").toString();
    node->code    = obj.value("code").toString();
    node->file    = obj.value("file").toString();
    node->line    = obj.value("line").toInt();
    node->col     = obj.value("col").toInt();

    node->symbolId = obj.value("symbolId").toString();
    node->varName  = obj.value("varName").toString();
    node->varType  = obj.value("varType").toString();
    node->isGlobal = obj.value("isGlobal").toBool();
    node->isLocal  = obj.value("isLocal").toBool();
    node->isParam  = obj.value("isParam").toBool();

    node->op             = obj.value("op").toString();
    node->calleeName     = obj.value("calleeName").toString();
    node->calleeSymbolId = obj.value("calleeSymbolId").toString();
    node->memberName     = obj.value("memberName").toString();

    if (!node->astId.isEmpty()) {
        model.astIndex.insert(node->astId, node);
    }

    const QJsonArray children = obj.value("children").toArray();
    node->children.reserve(children.size());
    for (const QJsonValue &vc : children) {
        if (!vc.isObject()) continue;
        node->children.push_back(parseAstNode(vc.toObject(), model));
    }

    return node;
}

static VarInfo parseVarLike(const QJsonObject &obj,
                            bool isGlobal,
                            bool isParam,
                            bool isLocal)
{
    VarInfo v;
    v.name     = obj.value("name").toString();
    v.symbolId = obj.value("symbolId").toString();
    v.type     = obj.value("type").toString();
    v.file     = obj.value("file").toString();
    v.line     = obj.value("line").toInt();
    v.isGlobal = isGlobal;
    v.isParam  = isParam;
    v.isLocal  = isLocal;
    return v;
}

bool buildProgramModel(const QJsonDocument &doc, ProgramModel &out)
{
    if (!doc.isObject()) return false;
    const QJsonObject root = doc.object();

    // 1) globalVars
    const QJsonArray gvs = root.value("globalVars").toArray();
    out.globalVars.reserve(gvs.size());
    for (const QJsonValue &v : gvs) {
        if (!v.isObject()) continue;
        VarInfo gv = parseVarLike(v.toObject(), /*isGlobal*/true, false, false);
        out.globalVars.push_back(gv);
        if (!gv.symbolId.isEmpty())
            out.varsBySymbolId.insert(gv.symbolId, gv);
    }

    // 2) functions
    const QJsonArray funcs = root.value("functions").toArray();
    out.functions.reserve(funcs.size());
    for (const QJsonValue &vf : funcs) {
        if (!vf.isObject()) continue;
        const QJsonObject fObj = vf.toObject();

        FunctionInfo f;
        f.name     = fObj.value("name").toString();
        f.symbolId = fObj.value("symbolId").toString();
        f.file     = fObj.value("file").toString();
        f.line     = fObj.value("line").toInt();

        // 2.1) params
        const QJsonArray params = fObj.value("params").toArray();
        for (const QJsonValue &vp : params) {
            if (!vp.isObject()) continue;
            VarInfo p = parseVarLike(vp.toObject(), false, /*isParam*/true, false);
            f.params.push_back(p);
            if (!p.symbolId.isEmpty())
                out.varsBySymbolId.insert(p.symbolId, p);
        }

        // 2.2) localVars
        const QJsonArray locals = fObj.value("localVars").toArray();
        for (const QJsonValue &vl : locals) {
            if (!vl.isObject()) continue;
            VarInfo lv = parseVarLike(vl.toObject(), false, false, /*isLocal*/true);
            f.localVars.push_back(lv);
            if (!lv.symbolId.isEmpty())
                out.varsBySymbolId.insert(lv.symbolId, lv);
        }

        // 2.3) blocks
        const QJsonArray blocks = fObj.value("blocks").toArray();
        f.blocks.reserve(blocks.size());
        for (const QJsonValue &vb : blocks) {
            if (!vb.isObject()) continue;
            const QJsonObject bObj = vb.toObject();

            BlockInfo b;
            b.id = bObj.value("id").toInt(-1);

            for (const QJsonValue &vp : bObj.value("predecessors").toArray())
                b.predecessors.push_back(vp.toInt());
            for (const QJsonValue &vs : bObj.value("successors").toArray())
                b.successors.push_back(vs.toInt());

            const QJsonArray stmts = bObj.value("stmts").toArray();
            b.stmts.reserve(stmts.size());
            for (const QJsonValue &vs : stmts) {
                if (!vs.isObject()) continue;
                const QJsonObject sObj = vs.toObject();

                StmtInfo s;
                s.astId = sObj.value("astId").toString();
                s.kind  = sObj.value("kind").toString();
                s.code  = sObj.value("code").toString();
                s.file  = sObj.value("file").toString();
                s.line  = sObj.value("line").toInt();
                s.col   = sObj.value("col").toInt();

                if (sObj.contains("tree") && sObj.value("tree").isObject()) {
                    s.tree = parseAstNode(sObj.value("tree").toObject(), out);
                }

                // 解析线程操作 threadOps（如果有的话）
                const QJsonArray tOps = sObj.value("threadOps").toArray();
                for (const QJsonValue &vt : tOps) {
                    if (!vt.isObject()) continue;
                    const QJsonObject tobj = vt.toObject();

                    ThreadOp op;
                    op.api               = tobj.value("api").toString();
                    op.kind              = tobj.value("kind").toString();
                    op.className         = tobj.value("className").toString();
                    op.threadVarName     = tobj.value("threadVarName").toString();
                    op.threadVarType     = tobj.value("threadVarType").toString();
                    op.threadVarId       = tobj.value("threadVarId").toInt(-1);
                    op.startFuncName     = tobj.value("startFuncName").toString();
                    op.startFuncSymbolId = tobj.value("startFuncSymbolId").toString();

                    s.threadOps.push_back(op);
                }

                b.stmts.push_back(std::move(s));
            }

            f.blocks.push_back(std::move(b));
        }

        out.functions.push_back(std::move(f));
    }

    return true;
}

bool loadProgramModelFromFile(const QString &path, ProgramModel &out)
{
    QFile file(path);
    if (!file.open(QIODevice::ReadOnly)) {
        util::logError(QStringLiteral("无法打开 analysis.json: %1").arg(path));
        return false;
    }

    const QByteArray bytes = file.readAll();
    file.close();

    QJsonParseError err{};
    const QJsonDocument doc = QJsonDocument::fromJson(bytes, &err);
    if (err.error != QJsonParseError::NoError) {
        util::logError(QStringLiteral("JSON 解析失败: %1 (%2)")
                           .arg(path, err.errorString()));
        return false;
    }

    return buildProgramModel(doc, out);
}

} // namespace analysis
