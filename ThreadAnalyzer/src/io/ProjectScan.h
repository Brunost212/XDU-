#ifndef PROJECTSCAN_H
#define PROJECTSCAN_H

#include <QModelIndex>
#include <QString>
#include <QList>

class QFileSyetemModel;

namespace io {
void collectFiles(const class QFileSystemModel* model,
                  const QModelIndex &parent,
                  QList<QString> &out);
}

#endif // PROJECTSCAN_H
