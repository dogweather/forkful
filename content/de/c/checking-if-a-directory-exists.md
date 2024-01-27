---
title:                "Überprüfen, ob ein Verzeichnis existiert"
date:                  2024-01-19
html_title:           "Arduino: Überprüfen, ob ein Verzeichnis existiert"
simple_title:         "Überprüfen, ob ein Verzeichnis existiert"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
(## Was & Warum?)
Überprüfen, ob ein Verzeichnis existiert, heißt einfach zu checken, ob ein bestimmter Pfad auf dem Dateisystem ein Ordner ist. Programmierer machen das, um Fehler zu vermeiden, bevor sie versuchen, Dateien zu lesen oder zu schreiben, oder das Verzeichnis selbst zu manipulieren.

## How to:
(## Wie geht das:)
Um zu prüfen, ob ein Verzeichnis in C existiert, verwenden wir die `stat()` Funktion aus der `<sys/stat.h>` Bibliothek.

```C
#include <stdio.h>
#include <sys/stat.h>

int directory_exists(const char *path) {
    struct stat statbuf;
    if (stat(path, &statbuf) != 0) {
        return 0; // Kann nicht auf das Verzeichnis zugreifen
    }

    // Überprüfe, ob es ein Verzeichnis ist
    return S_ISDIR(statbuf.st_mode);
}

int main() {
    const char *path = "/path/to/directory";

    if (directory_exists(path)) {
        printf("Das Verzeichnis existiert.\n");
    } else {
        printf("Das Verzeichnis existiert nicht.\n");
    }

    return 0;
}
```

Sample Output:
```
Das Verzeichnis existiert.
```
Oder:
```
Das Verzeichnis existiert nicht.
```

## Deep Dive:
(## Tiefer eintauchen:)
Die Funktion `stat()` gibt seit den frühen Unix-Tagen. Sie holt Datei-Informationen. Weil eine '0' für "falsch" in C steht, liefert `stat()` eine nicht-0 zurück, wenn es den Pfad nicht findet.

Es gibt auch `fstat()` und `lstat()` – `fstat()` arbeitet mit File-Deskriptoren, `lstat()` folgt keine symbolischen Links. Mit `dirent.h` gibt es eine andere Methode, die `opendir()` und `readdir()` verwendet, aber `stat()` ist kürzer für diesen Check.

In unseren Code, `S_ISDIR()` prüft, ob die Modus-Bits auf 'Verzeichnis' stellen – ein klares Indiz, dass der Pfad existiert und ein Verzeichnis ist.

## See Also:
(## Siehe auch:)
- Man-Page für `stat()`: https://linux.die.net/man/2/stat
- Stack Overflow Diskussionen über das Überprüfen der Verzeichnisse in C
- GNU C Library Dokumentation zu File System Utilities: https://www.gnu.org/software/libc/manual/html_node/File-System-Utilities.html

Bitte beachte, dass Online-Quellen und -Tools sich entwickeln und ändern können; checke immer das aktuelle Datum der Materialien, um sicher zu sein, dass die Infos noch korrekt sind.
