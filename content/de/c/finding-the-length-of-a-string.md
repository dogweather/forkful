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

## Was und Warum?
Das Finden der Länge einer Zeichenkette ist ein häufiger Schritt beim Programmieren in C. Es bezieht sich auf die Anzahl der Zeichen in einer Zeichenkette und kann hilfreich sein, um Informationen über den Inhalt einer Zeichenkette zu erhalten.

## Wie geht's?
Um die Länge einer Zeichenkette in C zu finden, können Sie die Funktion "strlen" verwenden. Hier ist ein Beispielcode:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Hallo!";
    int len = strlen(str);
    printf("Die Länge von str ist %d", len);
    return 0;
}
```

Das oben genannte Beispiel sollte die folgende Ausgabe erzeugen:

```
Die Länge von str ist 6
```

## Tiefentauchen
Die Funktion "strlen" wurde bereits im ersten ANSI-C-Standard im Jahr 1989 eingeführt. Sie ist Teil der C-Bibliothek "string.h". Alternativ können Sie auch die Funktion "sizeof" verwenden, um die Größe einer Zeichenkette zu finden. Dies ist jedoch nicht immer genau, da es auch nicht druckbare Zeichen in einer Zeichenkette geben kann. Die Implementierung von "strlen" kann variieren, aber in der Regel durchläuft sie die Zeichenkette und zählt die Anzahl der Zeichen, bis sie das Null-Zeichen erreicht.

## Siehe auch
- [strlen - C h-Befehlsreferenz](https://www.tutorialspoint.com/c_standard_library/c_function_strlen.htm)
- [sizeof - C h-Befehlsreferenz](https://www.tutorialspoint.com/c_standard_library/c_function_sizeof.htm)
- [Die C-Programmiersprache - Wikipedia](https://de.wikipedia.org/wiki/Die_C-Programmiersprache)