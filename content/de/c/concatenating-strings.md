---
title:                "Strings verketten"
html_title:           "Bash: Strings verketten"
simple_title:         "Strings verketten"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/concatenating-strings.md"
---

{{< edit_this_page >}}

# Strings in C Zusammenfügen: Eine Einführung

## Was und Warum?

Stringzusammenfügung ist das Verfahren, durch das mehrere Zeichenfolgen in eine einzige Zeichenfolge zusammengefügt werden. Programmierer tun dies, um Daten sinnvoll zu organisieren und zu manipulieren.

## Wie geht das?

Wir können die `strcat` Funktion aus der Bibliothek `string.h` verwenden, um Strings in C zusammenzufügen. Hier sehen Sie, wie das geht:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str1[50] = "Guten ";
    char str2[]   = "Tag!";
    
    strcat(str1, str2);
    printf("%s\n", str1);
    
    return 0;
}
```

Wenn Sie dieses Programm ausführen, sehen Sie folgenden Output:

```C
Guten Tag!
```

## Tiefgehende Informationen

Zur geschichtlichen Entwicklung, die Stringzusammenfügung ist schon seit den Anfangstagen der Programmierung ein zentrales Konzept, da Text oft das wichtigste Medium zur Interaktion mit Benutzern ist.

Es gibt andere Methoden, um Strings in C zu konkatenieren, z.B. die `sprintf` Funktion oder manuelles Verknüpfen mithilfe von Pointern.

Hinsichtlich der Implementation, `strcat` setzt das Ende des ersten Strings auf das Anfangszeichen des zweiten Strings und das Ende des zweiten Strings auf eine Null-Terminator. Es ist wichtig zu beachten, dass der erste String genügend Speicherplatz enthalten muss, um beide Strings aufzunehmen.

## Weitere Informationen

- Manual Page für `strcat`: http://man7.org/linux/man-pages/man3/strcat.3.html
- C Programming/String handling - Wikibooks: https://en.wikibooks.org/wiki/C_Programming/String_handling