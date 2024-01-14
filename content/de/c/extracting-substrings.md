---
title:    "C: Extrahieren von Teilzeichenketten"
keywords: ["C"]
---

{{< edit_this_page >}}

## Warum

Substring-Extrahierung ist ein wichtiger Teil der C-Programmierung, der es ermöglicht, Teilstrings aus einer größeren Zeichenkette zu extrahieren. Dies kann nützlich sein, um bestimmte Informationen oder Daten aus einem Text zu erhalten und sie weiter zu verarbeiten. In diesem Blog-Beitrag werden wir uns genauer damit befassen, wie man Substrings in C extrahieren kann.

## Wie man Substrings in C extrahiert

Um Substrings in C zu extrahieren, gibt es mehrere Funktionen, die wir nutzen können. Eine davon ist die Funktion `strncpy()`, die wir in Kombination mit der Funktion `strlen()` verwenden können, um die Länge des gewünschten Substrings zu bestimmen. In der folgenden Code-Schnipsel zeigen wir, wie wir die Funktionen zusammen nutzen können:

```C
#include <stdio.h>
#include <string.h>

int main() {
    // ursprüngliche Zeichenkette
    char *text = "Hallo, Welt!";

    // Buffer für den extrahierten Substring
    char substring[8];

    // Länge des Substrings bestimmen
    int length = strlen(text) - 1;

    // Substring extrahieren und in den Buffer kopieren
    strncpy(substring, text, length);

    // Output: "Hallo, W"
    printf("Output: \"%s\"\n", substring);

    return 0;
}
```

Wir verwenden `strlen()` hier, um die Länge des ursprünglichen Strings zu bestimmen, und ziehen dann 1 ab, um das Nullzeichen am Ende des Strings auszuschließen. In diesem Fall lautet die Länge des Substrings, den wir extrahieren möchten, also 8. Mit `strncpy()` kopieren wir dann den Substring in den zuvor deklarierten Buffer. Der Output dieses Codeschnipsels wäre also "Hallo, W", da wir die letzten beiden Zeichen (l und t) nicht in den Substring aufgenommen haben.

## Tiefergehende Informationen zur Substring-Extrahierung in C

Eine weitere Möglichkeit, Substrings in C zu extrahieren, ist die Funktion `strrchr()`, mit der man den letzten Vorkommensindex eines bestimmten Zeichens in einem String finden kann. In Kombination mit `strlen()` und `strncpy()` kann dies helfen, bestimmte Abschnitte aus einer Zeichenkette zu extrahieren.

Es gibt auch weitere Funktionen wie `strstr()` und `strpbrk()`, die bei der Suche und Extrahierung von Substrings hilfreich sein können. Es ist wichtig, sich mit diesen Funktionen vertraut zu machen und zu verstehen, wie sie zusammenarbeiten, um effizient Substrings zu extrahieren.

## Siehe auch

- [C - Strings](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [C String Funktionen](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [C-Programmierung: 7 Tipps für die Arbeit mit Strings](https://www.infoworld.com/article/3201732/c-programming-top-tips-for-dealing-with-strings.html)