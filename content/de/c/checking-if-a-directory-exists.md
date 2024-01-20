---
title:                "Überprüfen, ob ein Verzeichnis existiert"
html_title:           "C: Überprüfen, ob ein Verzeichnis existiert"
simple_title:         "Überprüfen, ob ein Verzeichnis existiert"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?

Prüfen, ob ein Verzeichnis existiert, ist eine grundlegende Funktion in der Programmierung, die es uns erlaubt, die Existenz eines spezifischen Verzeichnisses auf unserer Festplatte zu bestätigen. Programmierern ist dies wichtig, um eventuelle Fehler zu vermeiden, die auftreten können, wenn versucht wird, auf ein nicht existierendes Verzeichnis zuzugreifen oder Dateien darin zu speichern.

## Wie es geht:

In C können wir die Funktion `stat()` aus der Bibliothek `sys/stat.h` verwenden, um zu überprüfen, ob ein Verzeichnis existiert. Hier ist ein einfaches Beispiel:

```C
#include <sys/stat.h>
#include <stdio.h>

int main()
{
    struct stat stats;

    if (stat("C:\\Verzeichnis", &stats) == 0 && S_ISDIR(stats.st_mode))
        printf("Das Verzeichnis existiert.\n");
    else
        printf("Das Verzeichnis existiert nicht.\n");

    return 0;
}
```
Ausgabe:

```
Das Verzeichnis existiert nicht.
```

## Tiefer Tauchen:

Die `stat()` Funktion wurde ursprünglich in der Unix-Betriebssystemfamilie eingeführt und ist seitdem Teil der meisten C-Standardbibliotheken geworden. Es gibt auch Funktionen wie `access()`, `fstat()`, oder `lstat()` die alternativ verwendet werden können. Die `stat()` Funktion allerdings, gibt detaillierte Informationen über das Verzeichnis oder die Datei, einschließlich des Zeitstempels der letzten Modifikation, Größe, und ob es sich um ein Verzeichnis oder eine Datei handelt.

## Siehe Auch:

Für mehr Informationen schauen Sie diese Ressourcen an:

- [C Library - <sys/stat.h>](https://www.tutorialspoint.com/c_standard_library/sys_stat_h.htm)
- [C Programming Files I/O](https://www.programiz.com/c-programming/c-file-input-output)
- [Directory in C](https://www.geeksforgeeks.org/directory-in-c/)