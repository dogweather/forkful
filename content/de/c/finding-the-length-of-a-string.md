---
title:                "C: Die Länge einer Zeichenkette finden."
simple_title:         "Die Länge einer Zeichenkette finden."
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge eines Strings ist eine grundlegende Aufgabe, die jeder C Programmierer wissen sollte. Es ermöglicht einem, die Anzahl der Zeichen in einem String zu ermitteln und somit effektivere und präzisere Code zu schreiben.

## Wie man die Länge eines Strings in C findet

Es gibt mehrere Möglichkeiten, um die Länge eines Strings in C zu finden. Eine davon ist die Verwendung der Funktion `strlen()`, die in der Standard Library verfügbar ist. Diese Funktion nimmt einen String als Argument und gibt die Anzahl der Zeichen in dem String zurück.

Ein Beispiel für die Verwendung von `strlen()`:

```C
#include <stdio.h>
#include <string.h>

int main() {

    char string[] = "Hallo!";
    int length = strlen(string);
    printf("Die Länge des Strings ist %d.\n", length);

    return 0;
}
```

Die Ausgabe dieses Codes wäre:

```
Die Länge des Strings ist 6.
```

Eine andere Methode ist das Durchlaufen des Strings mit einer Schleife und Zählen der Anzahl der Zeichen. Dies kann nützlich sein, wenn Sie zusätzliche Bedingungen für die Länge des Strings haben möchten.

Ein Beispiel für diese Methode:

```C
#include <stdio.h>

int main() {

    char string[] = "Guten Morgen!";
    int length = 0;

    while(string[length] != '\0') {
        length++;
    }

    printf("Die Länge des Strings ist %d.\n", length);

    return 0;
}
```

Die Ausgabe würde hier auch 13 sein.

## Tiefere Einblicke

Es ist wichtig zu beachten, dass der Wert, den `strlen()` zurückgibt, die Anzahl der Zeichen ohne das Null-Terminator-Zeichen (`\0`) am Ende des Strings ist. Dieses Zeichen markiert das Ende des Strings, hat aber keine Auswirkungen auf die Anzahl der Zeichen im String.

Es gibt auch Funktionen wie `strnlen()`, die eine maximale Anzahl von Zeichen angeben können, die in dem String gezählt werden sollen.

Es ist wichtig, sicherzustellen, dass der angegebene String gültig ist, bevor Sie versuchen, die Länge zu finden, da dies zu unerwarteten Ergebnissen oder sogar zu Programmabstürzen führen kann.

## Siehe auch

- [Offizielle Dokumentation zu strlen()](https://www.cplusplus.com/reference/cstring/strlen/)
- [Weitere Möglichkeiten, die Länge eines Strings in C zu finden](https://www.geeksforgeeks.org/program-find-length-string/)
- [Eine kurze Einführung in C-Strings](https://www.freecodecamp.org/news/c-strings-beginners/)