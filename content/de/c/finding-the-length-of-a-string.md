---
title:                "Die Länge eines Strings finden"
html_title:           "C: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum
Das Finden der Länge eines Strings ist eine häufige Aufgabe in der Programmierung, die für verschiedene Anwendungen erforderlich sein kann. Es kann hilfreich sein, um zu überprüfen, ob eine Eingabe innerhalb einer bestimmten Grenze liegt oder um Daten in einem Leseprozess zu verarbeiten.

## Wie geht man vor
Das Finden der Länge eines Strings in C ist relativ einfach. Zunächst muss ein String definiert werden, danach kann die Länge mit der Funktion `strlen()` berechnet werden. Hier ist ein Beispielcode, der einen String definiert und die Länge mit Hilfe der `strlen()` Funktion ausgibt:

```C
#include <stdio.h>
#include <string.h>

int main()
{
    // String definieren
    char str[] = "Hallo Welt";

    // Länge ausgeben
    printf("Die Länge des Strings ist %d\n", strlen(str));

    return 0;
}
```
Das Ergebnis des obigen Codes ist `Die Länge des Strings ist 10`. Beachten Sie, dass die Länge des Strings die Anzahl der Zeichen im String einschließlich des Nullterminators ist.

## Tiefer gehende Informationen
Das Finden der Länge eines Strings kann auch manuell erfolgen, indem man durch den String läuft und die Anzahl der Zeichen zählt. Dies kann jedoch zeitaufwändig sein, besonders wenn der String sehr lang ist. Die `strlen()` Funktion ist in solchen Fällen die effizientere Wahl.

Eine weitere wichtige Sache zu beachten ist, dass die `strlen()` Funktion die Länge des Strings berechnet, solange der String richtig terminiert ist. Ist dies nicht der Fall, kann die Funktion falsche Ergebnisse liefern oder sogar zu einem Programmabsturz führen. Daher ist es wichtig, sicherzustellen, dass alle Strings ordnungsgemäß mit einem Nullterminator terminiert sind.

## Siehe auch
- Offizielle Dokumentation der `strlen()` Funktion in C: https://www.cplusplus.com/reference/cstring/strlen/
- Ein Tutorial zum Finden der Länge eines Strings in C: https://www.programiz.com/c-programming/examples/measure-string-length