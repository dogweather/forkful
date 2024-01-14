---
title:                "C: Überprüfen, ob ein Verzeichnis existiert"
simple_title:         "Überprüfen, ob ein Verzeichnis existiert"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie sich mit der Programmiersprache C beschäftigen, kann es vorkommen, dass Sie überprüfen müssen, ob ein bestimmter Verzeichnispfad existiert oder nicht. Dies kann nützlich sein, um sicherzustellen, dass Ihr Programm korrekt funktioniert und keine unerwarteten Fehler auftreten. In diesem Artikel werden wir besprechen, wie Sie in C überprüfen können, ob ein Verzeichnis existiert.

## Wie geht es

Um zu überprüfen, ob ein Verzeichnis existiert, können Sie die Funktion `opendir()` aus der Standardbibliothek `dirent.h` verwenden. Diese Funktion öffnet einen Verzeichnisstrom und gibt einen Zeiger auf den Strom zurück. Wenn der Verzeichnisstrom erfolgreich geöffnet wurde, wird kein NULL-Wert zurückgegeben. Andernfalls wird NULL zurückgegeben und Sie können anhand dieser Rückgabe entscheiden, ob das Verzeichnis existiert oder nicht.

```C
#include <stdio.h>
#include <dirent.h>

int main() {
    // Öffnen des Verzeichnisstroms
    DIR *dir = opendir("/home/mydir");
    
    // Überprüfen, ob der Verzeichnisstrom erfolgreich geöffnet wurde
    if (dir != NULL) {
        printf("Das Verzeichnis existiert! \n");
        // Schließen des Verzeichnisstroms
        closedir(dir);
    } else {
        printf("Das Verzeichnis existiert nicht! \n");
    }
    
    return 0;
}
```

Die Ausgabe dieses Beispiels sollte "Das Verzeichnis existiert nicht!" sein, da wir hier versuchen, auf das Verzeichnis "/home/mydir" zuzugreifen, das möglicherweise nicht vorhanden ist.

## Tiefer eintauchen

Es ist wichtig zu beachten, dass die Überprüfung, ob ein Verzeichnis existiert, nicht unbedingt bedeutet, dass das Verzeichnis auch tatsächlich zugänglich ist. Möglicherweise fehlen bestimmte Berechtigungen oder das System kann aus anderen Gründen nicht auf das Verzeichnis zugreifen. Daher ist es ratsam, zusätzliche Überprüfungen durchzuführen, um sicherzustellen, dass das Verzeichnis nicht nur existiert, sondern auch zugänglich ist.

Eine Möglichkeit, dies zu tun, ist die Verwendung der Funktion `access()` aus der Headerdatei `unistd.h`. Diese Funktion ermöglicht es Ihnen, Berechtigungen für einen bestimmten Pfad zu überprüfen. Wenn Sie beispielsweise überprüfen möchten, ob ein bestimmtes Verzeichnis lesbar ist, könnten Sie Folgendes tun:

```C
#include <stdio.h>
#include <unistd.h>

int main() {
    // Überprüfen, ob Verzeichnis lesbar ist
    if (access("/home/mydir", R_OK) == 0) {
        printf("Das Verzeichnis ist lesbar!");
    } else {
        printf("Das Verzeichnis ist nicht lesbar!");
    }
    
    return 0;
}
```

## Siehe auch

- Die offizielle Dokumentation der Funktion `opendir()`: https://man7.org/linux/man-pages/man3/opendir.3.html
- Weitere Informationen zur Funktion `access()`: https://linux.die.net/man/2/access
- Eine Anleitung zur Verwendung von Datei- und Verzeichnisfunktionen in C: https://www.cprogramming.com/tutorial/c/lesson9.html