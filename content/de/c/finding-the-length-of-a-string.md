---
title:                "Die Länge eines Strings ermitteln"
html_title:           "Java: Die Länge eines Strings ermitteln"
simple_title:         "Die Länge eines Strings ermitteln"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Längenermittlung eines Zeichenketten (Strings) in C berechnet die Anzahl der Zeichen in einer Zeichenkette. Dies ist nützlich, um den Speicherbedarf zu ermitteln oder um durch die Zeichenkette zu iterieren.

## So geht's:
Die Standardfunktion `strlen()` aus der Bibliothek `<string.h>` hilft uns, die Länge eines Strings zu ermitteln. Hier ist ein einfaches Beispiel:

```C 
#include <stdio.h>
#include <string.h>

int main()
{
     char str[] = "Hallo, Welt!";
     int len = strlen(str);
     printf("Die Länge des Strings ist %d", len);
     return 0;
}
```

Laufen Sie das Programm und Sie werden "Die Länge des Strings ist 13" sehen.

## Deep Dive
`strlen()` wurde in der ursprünglichen ANSI C Standardbibliothek eingeführt, um die Länge eines nullterminierten Strings zu bestimmen. Intern zählt es die Anzahl der Zeichen bis zum nullterminierenden Zeichen (`'\0'`) und gibt diesen Wert zurück.

Eine Alternative zur `strlen()` Funktion ist das manuelle Durchlaufen des Strings, bis das nullterminierende Zeichen erreicht wird:

```C
int laenge(char *str)
{
    int len = 0;
    while (*str != '\0') 
    {
       str++;
       len++;
    }
    return len;
}
```

Beachten Sie, dass die Implementierung von `strlen()` hardwareabhängig beschleunigt werden kann, und ist daher normalerweise schneller als eine manuelle Zählung.

## Siehe auch
Weitere Informationen finden Sie unter diesen Links:
1. [C string handling](https://en.wikipedia.org/wiki/C_string_handling)
2. [C Standard Library](https://en.wikipedia.org/wiki/C_standard_library)