---
title:    "C: Überprüfen, ob ein Verzeichnis vorhanden ist"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Das Überprüfen, ob ein Verzeichnis existiert, kann für Programmierer in vielen Situationen nützlich sein. Zum Beispiel, um sicherzustellen, dass ein bestimmtes Verzeichnis vorhanden ist, bevor eine Datei erstellt oder gelöscht wird. Es kann auch dazu dienen, präzisere Fehlermeldungen in einem Programm zu erstellen, wenn ein erwartetes Verzeichnis fehlt.

## Wie geht es

Um zu überprüfen, ob ein Verzeichnis existiert, können wir die Funktion `opendir` in Kombination mit dem `struct stat` verwenden. Hier ist ein Beispielcode in C:

```C
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <stdio.h>
#include <string.h>

int main() {
  // Das Verzeichnis, das wir überprüfen möchten
  char* verzeichnis = "/home/benutzer/meinVerzeichnis/";

  // Überprüfung, ob Verzeichnis existiert
  struct stat stats;
  if (stat(verzeichnis, &stats) == 0) {
    printf("Das Verzeichnis existiert!\n");
  } else {
    printf("Das Verzeichnis existiert nicht!\n");
  }

  // Überprüfung mit opendir
  DIR* dir = opendir(verzeichnis);
  if (dir) {
    printf("Das Verzeichnis existiert!\n");
    closedir(dir);
  } else {
    printf("Das Verzeichnis existiert nicht!\n");
  }

  return 0;
}
```

Beispiel Ausgabe:

```
Das Verzeichnis existiert!
Das Verzeichnis existiert!
```

## Deep Dive

Die Funktion `opendir` versucht, ein Verzeichnis mit dem angegebenen Pfad zu öffnen, falls es existiert. Wenn nicht, gibt sie `NULL` zurück. `stat` andererseits gibt uns die Möglichkeit, zusätzliche Informationen zu dem bestimmten Dateiobjekt, in diesem Fall dem Verzeichnis, zu erhalten. Wir können dann die `st_mode` Eigenschaft des `struct stat` verwenden, um sicherzustellen, dass es sich tatsächlich um ein Verzeichnis handelt und nicht um eine andere Art von Datei.

## Siehe auch

- [opendir Funktion in der C-Dokumentation](https://www.gnu.org/software/libc/manual/html_node/Opening-a-Directory.html)
- [stat Funktion in der C-Dokumentation](https://www.gnu.org/software/libc/manual/html_node/File-Status-Function-Example.html)
- [Struct-Stat Referenz in der C-Dokumentation](https://www.gnu.org/software/libc/manual/html_node/Stat-Functions.html)