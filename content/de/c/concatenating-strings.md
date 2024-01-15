---
title:                "Zeichenfolgen verbinden"
html_title:           "C: Zeichenfolgen verbinden"
simple_title:         "Zeichenfolgen verbinden"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Wenn du jemals Text aus verschiedenen Variablen oder Werten zusammenfügen musst, bist du auf die String-Konkatenation angewiesen. Diese Funktion ermöglicht es, mehrere Strings zu einem größeren String zu verbinden, was in vielen Anwendungen sehr nützlich sein kann.

## Wie man es macht

Die Verkettung von Strings in C ist recht einfach. Du musst lediglich die ```strcat``` Funktion verwenden, die in der C-Standardbibliothek enthalten ist. Hier ist ein Beispiel:

```
char str1[20] = "Hallo";
char str2[20] = "Welt";
strcat(str1, str2);
printf("%s", str1);
```

Die Ausgabe dieses Codes wäre "HalloWelt". Wie du sehen kannst, fügt ```strcat``` den Inhalt von ```str2``` an das Ende von ```str1``` an.

## Tiefentauchen

Es gibt auch andere Wege, Strings in C zu verkettet. Eine andere Möglichkeit ist die Verwendung des Operators "+" anstatt der ```strcat``` Funktion. Hier ist ein Beispiel:

```
#include <stdio.h>
#include <string.h>

int main() {
   char str1[20] = "Hallo";
   char str2[20] = "Welt";
   char str3[40];
   
   str3[0] = '\0';
   strcat(str3, str1);
   strcat(str3, str2);

   printf("%s", str3);
   
   return 0;
}
```

Die Ausgabe wäre immer noch "HalloWelt". Der Operator "+" fügt jedoch nur eine einzelne Zeichenkette an das Ende einer bestehenden Zeichenkette an, während ```strcat``` es ermöglicht, mehrere Zeichenketten zusammen zu fügen.

## Siehe auch

- [C-String-Konkatenation auf tutorialspoint.com](https://www.tutorialspoint.com/cprogramming/c_string_concatenation.htm)
- [C-String-Manipulation auf Wikibooks](https://en.wikibooks.org/wiki/C_Programming/String_manipulation)