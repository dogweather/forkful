---
title:                "Überprüfung, ob ein Verzeichnis existiert"
aliases:
- /de/c/checking-if-a-directory-exists/
date:                  2024-02-03T17:52:35.000398-07:00
model:                 gpt-4-0125-preview
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Das Überprüfen, ob ein Verzeichnis in C existiert, beinhaltet die Abfrage des Dateisystems, um zu verifizieren, ob ein spezifischer Pfad zu einem Verzeichnis führt. Programmierer führen diese Operation oft durch, um sicherzustellen, dass Dateioperationen (wie Lesen von oder Schreiben in Dateien) auf gültige Pfade gerichtet sind, um Fehler zu verhindern und die Softwarezuverlässigkeit zu erhöhen.

## Wie:

In C kann die Existenz eines Verzeichnisses mit der `stat` Funktion überprüft werden, welche Informationen über die Datei oder das Verzeichnis an einem bestimmten Pfad abruft. Das `S_ISDIR` Makro von `sys/stat.h` wird dann verwendet, um zu bewerten, ob die abgerufenen Informationen einem Verzeichnis entsprechen.

So können Sie `stat` und `S_ISDIR` verwenden, um zu überprüfen, ob ein Verzeichnis existiert:

```c
#include <stdio.h>
#include <sys/stat.h>

int main() {
    struct stat stats;
    
    // Pfad des zu überprüfenden Verzeichnisses
    char *dirPath = "/path/to/directory";

    // Den Status des Pfades erhalten
    int result = stat(dirPath, &stats);

    // Überprüfen, ob das Verzeichnis existiert
    if (result == 0 && S_ISDIR(stats.st_mode)) {
        printf("Das Verzeichnis existiert.\n");
    } else {
        printf("Das Verzeichnis existiert nicht.\n");
    }

    return 0;
}
```

Beispielausgabe:
```
Das Verzeichnis existiert.
```

Oder, wenn das Verzeichnis nicht existiert:
```
Das Verzeichnis existiert nicht.
```

## Vertiefung:

Die `stat` Struktur und Funktion sind seit Jahrzehnten Teil der C-Programmiersprache und stammen aus Unix. Sie bieten eine standardisierte Methode, um Informationen des Dateisystems abzurufen, welche, obwohl sie relativ niedriges Niveau sind, aufgrund ihrer Einfachheit und des direkten Zugriffs auf die Metadaten des Dateisystems weit verbreitet verwendet werden.

Historisch gesehen war das Überprüfen der Existenz und Eigenschaften von Dateien und Verzeichnissen mit `stat` und dessen Derivaten (wie `fstat` und `lstat`) ein üblicher Ansatz. Allerdings interagieren diese Funktionen direkt mit dem Betriebssystem-Kernel, was Overhead und potenzielle Fehler verursachen kann, wenn sie nicht korrekt gehandhabt werden.

Für neue Projekte oder wenn in hochrangigen Szenarien gearbeitet wird, könnten Programmierer optieren für stärker abstrahierte Dateiverarbeitungsmechanismen, die von modernen Frameworks oder Bibliotheken bereitgestellt werden, die Fehler eleganter behandeln und eine einfachere API zur Verfügung stellen. Dennoch bleibt das Verständnis und die Fähigkeit, `stat` zu verwenden, eine wertvolle Fähigkeit für Szenarien, die direkte Dateisystemmanipulation erfordern, wie Systemprogrammierung oder wenn man in eingeschränkten Umgebungen arbeitet, in denen Abhängigkeiten von großen Bibliotheken nicht durchführbar sind.
