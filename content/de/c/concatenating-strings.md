---
title:    "C: Verkettung von Strings"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c/concatenating-strings.md"
---

{{< edit_this_page >}}

# Warum

Das Zusammenführen oder Verkettung von Zeichenketten, auch bekannt als String-Konkatenation, ist eine grundlegende Funktion in C-Programmierung. Es ermöglicht dem Benutzer, verschiedene Strings zu kombinieren, um einen größeren String zu erstellen. Diese Funktion ist besonders nützlich, wenn Sie Texte in Ihrem Programm dynamisch generieren müssen. 

# Wie man

Die Syntax für die Verkettung von Strings in C ist relativ einfach. Man muss nur `strcat()` verwenden, gefolgt vom Ziel- und Quellenstring. Hier ist ein Beispiel, um es besser zu verstehen:

```c
char destination[50] = "Hallo ";
char source[] = "Welt";
strcat(destination, source);
printf("%s", destination);
```

**Ausgabe:** Hallo Welt

Wie Sie sehen können, wird der Quellenstring "Welt" an das Ende des Zielstrings "Hallo" angehängt. Beachten Sie, dass der Zielstring groß genug sein muss, um die zusätzlichen Zeichen aufzunehmen, da sonst ein Pufferüberlauf auftreten könnte.

# Tief eintauchen

Wenn man sich die zugrundeliegende Technik der Zeichenkettenverkettung ansieht, kann es etwas komplexer erscheinen. In Wirklichkeit verwendet die Funktion `strcat()` die Funktion `strlen()` (Stringlänge) und `strcpy()` (Zeichenkette kopieren), um die Zeichenketten zu verbinden. Hier ist ein Beispiel, das dies verdeutlicht:

```c
char destination[50] = "Hallo ";
char source[] = "Welt";
strcat(destination, source);
printf("%s\n", destination);
printf("Die Länge des Zielstrings beträgt: %d\n", strlen(destination));
printf("Die Länge des Quellstrings beträgt: %d\n", strlen(source));
```

**Ausgabe:**
Hallo Welt
Die Länge des Zielstrings beträgt: 11
Die Länge des Quellstrings beträgt: 5

Man kann sehen, dass die Länge des Zielstrings der Summe der Längen von Ziel- und Quellstrings entspricht. Die Funktion `strcpy()` kopiert den Inhalt des Quellstrings in den Zielstring und die Funktion `strlen()` zählt die Anzahl der Zeichen, um die Länge zu bestimmen.

# Siehe auch 

- [`strcat()` Dokumentation von cplusplus.com](http://www.cplusplus.com/reference/cstring/strcat/)
- [`strcat()` Tutorial von tutorialspoint.com](https://www.tutorialspoint.com/c_standard_library/c_function_strcat.htm)
- [Weitere Beispiele von String-Konkatenation](https://www.programiz.com/c-programming/examples/concatenate-string)