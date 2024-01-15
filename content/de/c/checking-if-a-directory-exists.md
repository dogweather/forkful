---
title:                "Überprüfung der Existenz eines Verzeichnisses"
html_title:           "C: Überprüfung der Existenz eines Verzeichnisses"
simple_title:         "Überprüfung der Existenz eines Verzeichnisses"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte man überhaupt prüfen, ob ein Verzeichnis existiert? Diese Frage mag sich der ein oder andere vielleicht schon gestellt haben. Die Antwort ist einfach: Wenn man in seinem Programm auf Dateien zugreift, ist es wichtig, sicherzustellen, dass das Verzeichnis, in dem sich die Dateien befinden, auch tatsächlich existiert.

## Wie geht man vor?
Die Überprüfung, ob ein Verzeichnis existiert, ist in C nicht kompliziert. Wir verwenden dazu die Funktion `opendir()` aus der `<dirent.h>`-Bibliothek. Hier ein Beispiel:
```C
#include <stdio.h>
#include <dirent.h>

int main() {
    DIR *dir = opendir("/Pfad/zum/Verzeichnis");
    if (dir) {
        printf("Das Verzeichnis existiert.\n");
        closedir(dir);
    } else {
        printf("Das Verzeichnis existiert nicht.\n");
    }
    return 0;
}
```
Wenn das Verzeichnis existiert, wird die Nachricht "Das Verzeichnis existiert." ausgegeben. Andernfalls wird die Nachricht "Das Verzeichnis existiert nicht." angezeigt. 
Die Funktion `opendir()` gibt einen Zeiger auf eine Struktur vom Typ `DIR` zurück, wenn das Verzeichnis existiert. Ist das Verzeichnis nicht vorhanden, wird `NULL` zurückgegeben.

## Tiefer Einblick
Um zu verstehen, wie die Funktion `opendir()` funktioniert, ist es wichtig, sich mit dem Dateisystem auf einem Computer vertraut zu machen. Ein Verzeichnis ist einfach gesagt eine Sammlung von Dateien und Unterverzeichnissen. Die Funktion `opendir()` versucht, das angegebene Verzeichnis zu öffnen und gibt einen Zeiger auf diese Verzeichnisstruktur zurück, wenn es existiert. Ansonsten wird `NULL` zurückgegeben und das Programm kann entsprechend darauf reagieren.

## Siehe auch
- [Dokumentation zur `opendir()`-Funktion](https://www.tutorialspoint.com/c_standard_library/c_function_opendir.htm)
- [Einführung in das Dateisystem und Verzeichnisstrukturen](https://www.lifewire.com/understanding-directories-and-subdirectories-1166195)
- [Beispielprogramm zur Überprüfung, ob ein Verzeichnis existiert](https://www.educative.io/edpresso/how-to-check-if-a-directory-exists-in-c)