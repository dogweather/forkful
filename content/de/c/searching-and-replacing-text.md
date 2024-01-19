---
title:                "Suchen und Ersetzen von Text"
html_title:           "Bash: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Was & Warum?

Suchen und Ersetzen von Text ist eine Operation, bei der ein bestimmter Text (das "Suchmuster") gefunden und durch einen anderen (den "Ersatz") ersetzt wird. Programmierer tun dies häufig, um redundante Aufgaben zu automatisieren oder definierte Muster im Code zu manipulieren.

## So geht's:

Die C-Standardbibliothek bietet `strstr` und `sprintf` Funktionen zum Suchen und Ersetzen von Texten. Hier ist ein kurzes Beispiel:

```C 
#include<stdio.h>
#include<string.h>

char* suchen_und_ersetzen(char* str, char* alt_text, char* neu_text) {
    static char buffer[4096];
    char *p;
 
    if(!(p = strstr(str, alt_text)))
        return str;

    strncpy(buffer, str, p-str);
    buffer[p-str] = '\0';

    sprintf(buffer+(p-str), "%s%s", neu_text, p+strlen(alt_text));

    return buffer;
}

int main() {
    char str[] = "Hallo Welt!";
    char alt_text[] = "Welt";
    char neu_text[] = "Zuhause";
   
    printf("%s\n", suchen_und_ersetzen(str, alt_text, neu_text));

    return 0;
}
```

Der Output wird `"Hallo Zuhause!"` sein.

## Vertiefende Infos 

Historisch gesehen war die Textsuch- und Ersetztechnik bereits in frühen Texteditoren wie `ed` und `vi` implementiert. Alternative Methoden zum Suchen und Ersetzen von Text können mit fortschrittlicheren Regex oder KMP-Algorithmen erreicht werden. Da C keine eingebaute Unterstützung für String-Manipulation bietet, verwenden wir hier ein manuelles Verfahren zum Finden und Ersetzen von Text, was einige Einschränkungen aufweist, wie z.B. die Begrenzung der Größe des Buffers.

## Siehe auch

1. [GNU C Library Documentation: String and Array Utilities](https://www.gnu.org/software/libc/manual/html_node/String-and-Array-Utilities.html)
2. [C Programming/stdlib.h/strstr](https://en.wikibooks.org/wiki/C_Programming/string.h/strstr)
3. [Tutorialspoint - C Programming - strstr](https://www.tutorialspoint.com/c_standard_library/c_function_strstr.htm)