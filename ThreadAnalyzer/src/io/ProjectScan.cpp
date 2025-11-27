#include "ProjectScan.h"
#include <QFileSystemModel>
#include <QString>
#include <QDebug>

namespace io{
void collectFiles(const ::QFileSystemModel* model,
                  const QModelIndex &parent,
                  QList<QString> &out)
{
    if(!model || !parent.isValid())return;

    int rows = model->rowCount(parent);
    for(int i = 0; i < rows; i++)
    {
        QModelIndex idx = model->index(i,0,parent);
        if(!idx.isValid())continue;

        QString path = model->filePath(idx);
        if(model->isDir(idx))collectFiles(model,idx,out);
        else
        {
            qDebug() << "正在加载" << path;
            out << path;
        }
    }
}

}

