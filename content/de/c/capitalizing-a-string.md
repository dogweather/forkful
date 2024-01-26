---
title:                "String in Großbuchstaben umwandeln"
html_title:           "C: String in Großbuchstaben umwandeln"
simple_title:         "String in Großbuchstaben umwandeln"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
In C geht's um's Großschreiben von Strings: aus 'hello' wird 'HELLO'. Programmierer nutzen das, um Text konsistent darzustellen oder Codes zu standardisieren.

## How to:
Capitalizing strings in C ist unkompliziert. Hier ist ein simples Beispiel:

```c
#include <stdio.h>
#include <ctype.h>

void capitalize(char *str) {
    while (*str) {
        *str = toupper((unsigned char)*str);
        str++;
    }
}

int main() {
    char text[] = "Guten Tag, Welt!";
    capitalize(text);
    printf("%s\n", text);  // Output: GUTEN TAG, WELT!
    return 0;
}
```

Die `toupper`-Funktion aus ctype.h wandelt jeden Buchstaben in Großbuchstaben um. Durchlaufe den String, fertig.

## Deep Dive
In den alten Zeiten (denken wir an C89), mussten Programmierer eigene Funktionen schreiben, um Buchstaben zu konvertieren. Heute machen Bibliotheken wie `ctype.h` die Arbeit leichter.

Alternativ könnten wir uns ganz modern auch `for`-Loops und die Funktion `toupper` zunutze machen.
 
C hat keine eingebaute String-Typ wie in höheren Sprachen. Strings sind hier einfach Arrays von chars. Deswegen ist eine Funktion wie `capitalize` nötig, die das Array char für char durchgeht.

Noch was: Im Unicode-Zeitalter kann es komplizierter sein, wenn es um Zeichen außerhalb des ASCII-Bereichs geht. Da braucht man vielleicht umfangreichere Bibliotheken, die Mehrsprachen-Support bieten.

## See Also
Wer tiefer graben möchte:

- C Standard Library documentation: https://en.cppreference.com/w/c/header
- ASCII Tabelle für die Übersicht der char Werte: http://www.asciitable.com/
- Für Unicode und mehrsprachige Unterstützung: http://site.icu-project.org/
