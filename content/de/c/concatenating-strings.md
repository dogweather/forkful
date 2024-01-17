---
title:                "Das Verbinden von Zeichenketten"
html_title:           "C: Das Verbinden von Zeichenketten"
simple_title:         "Das Verbinden von Zeichenketten"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?
Beim Programmieren geht es oft darum, verschiedene Daten und Variablen zu verbinden, um eine größere und nützlichere Einheit zu bilden. Eine Möglichkeit, dies zu tun, ist das Verketten von Strings, also das Zusammenfügen mehrerer Zeichenketten zu einer einzigen. Dies ist besonders hilfreich, wenn wir Texte oder Nachrichten erstellen möchten, die aus verschiedenen Teilen bestehen.

## Wie geht's?
Um Strings in C zu verketten, benötigen wir die Standardbibliotheksfunktion ```strcat()```. Diese Funktion nimmt zwei Strings als Argumente und fügt den zweiten String an das Ende des ersten Strings an. Hier ist ein Beispiel:

```
char str1[] = "Hallo, ";
char str2[] = "Welt!";
strcat(str1, str2);
printf("%s", str1);
//Ausgabe: Hallo, Welt!
```

## Tiefere Einblicke
Das Verketten von Strings ist eine häufig verwendete Methode, die bereits seit den frühen Tagen der Programmierung existiert. Früher wurden Strings als Arrays von Zeichen behandelt, was bedeutete, dass jede Zeichenposition einzeln manipuliert werden musste, um sie zu verketten. Heutzutage gibt es alternative Methoden wie die Verwendung von Zeigern oder das Verwenden von Funktionen aus anderen Bibliotheken wie ```sprintf()```.

## Sieh auch
- [The strcat() function in C](https://www.programiz.com/c-programming/library-function/string.h/strcat)
- [C String Handling Functions](https://www.tutorialspoint.com/c_standard_library/string_h.htm)