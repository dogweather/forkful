---
title:                "Überprüfen, ob ein Verzeichnis existiert."
html_title:           "C++: Überprüfen, ob ein Verzeichnis existiert."
simple_title:         "Überprüfen, ob ein Verzeichnis existiert."
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum
Es gibt Situationen, in denen man überprüfen muss, ob ein bestimmtes Verzeichnis existiert, bevor man damit arbeiten kann. Zum Beispiel, wenn man ein Programm schreibt, das bestimmte Dateien in einem spezifischen Verzeichnis suchen und öffnen soll. Das Überprüfen der Existenz eines Verzeichnisses ist somit ein wichtiger Schritt in der Programmierung.

## Wie funktioniert es
Es gibt verschiedene Möglichkeiten, um in C++ zu überprüfen, ob ein Verzeichnis existiert. Eine Möglichkeit ist die Verwendung der Funktion `opendir()` aus der Bibliothek `<dirent.h>`. Diese Funktion öffnet ein Verzeichnis und gibt einen Zeiger auf das geöffnete Verzeichnis zurück, wenn es existiert. Ansonsten wird `NULL` zurückgegeben.

```C++
#include <dirent.h>
#include <iostream>

int main() {
    // Öffne das Verzeichnis "beispiel" und speichere den Zeiger im Verzeichnis "dir"
    DIR* dir = opendir("beispiel"); 

    // Überprüfe, ob das Verzeichnis geöffnet werden konnte
    if (dir != NULL) {
        // Das Verzeichnis existiert und der Zeiger im Verzeichnis "dir" ist gültig
        std::cout << "Das Verzeichnis existiert!" << std::endl;
        // Schließe das Verzeichnis wieder
        closedir(dir);
    } else {
        // Das Verzeichnis existiert nicht oder es gab einen Fehler beim Öffnen
        std::cout << "Das Verzeichnis existiert nicht!" << std::endl;
    }

    return 0;
}
```

Die Funktion `opendir()` ist jedoch nicht die einzige Möglichkeit, um zu überprüfen, ob ein Verzeichnis existiert. Man kann auch die Funktion `stat()` aus der Bibliothek `<sys/stat.h>` verwenden, um Informationen über ein bestimmtes Verzeichnis zu erhalten. Wenn das Verzeichnis existiert, wird in der Struktur `stat` ein spezifisches Flag gesetzt, das angibt, dass es sich um ein Verzeichnis handelt.

```C++
#include <sys/stat.h>
#include <iostream>

int main() {
    // Struktur für die Informationen über ein Verzeichnis
    struct stat info;
    // Versuche, Informationen über das Verzeichnis "beispiel" anzufordern
    int result = stat("beispiel", &info);

    // Überprüfe, ob die Information erfolgreich angefordert wurde
    if (result == 0) {
        // Überprüfe das spezifische Flag für Verzeichnisse
        if (info.st_mode & S_IFDIR) {
            // Das Verzeichnis existiert
            std::cout << "Das Verzeichnis existiert!" << std::endl;
        }
    } else {
        // Das Verzeichnis existiert nicht oder es gab einen Fehler bei der Anforderung
        std::cout << "Das Verzeichnis existiert nicht!" << std::endl;
    }

    return 0;
}
```

## Ausführliche Erklärung
Beide Methoden, ob mit `opendir()` oder `stat()`, überprüfen letztendlich nur, ob ein Verzeichnis existiert, jedoch gibt es einige Unterschiede bei ihrer Anwendung. Die Funktion `opendir()` ist Teil der C-Standardbibliothek und somit plattformunabhängig. Sie ist daher eine gute Wahl für portable Programme. Die Funktion `stat()` hingegen ist plattformspezifisch und funktioniert möglicherweise nicht auf allen Betriebssystemen. Andererseits liefert sie auch weitere Informationen über ein Verzeichnis, wie z.B. die Größe oder das Erstellungsdatum.

## Siehe auch
- [Verzeichnis öffnen mit opendir() (en)](https://www.geeksforgeeks.org/opendir-in-c-with-examples/)
- [Informationen über ein Verzeichnis mit stat() (en)](https://www.linuxnix.com/c-stat-function-usage-with-examples/)
- [Blog zu C++-Programmierung (de)](https://www.fluentcpp.com)