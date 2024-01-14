---
title:                "C: Die Länge eines Strings finden"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Warum
Die Länge einer Zeichenkette zu finden, ist eine grundlegende Aufgabe beim Programmieren. Es ermöglicht uns, dynamisch mit Zeichenketten umzugehen und sie in unserem Code effizient zu verarbeiten. In diesem Blogbeitrag gehen wir Schritt für Schritt durch, wie man die Länge einer Zeichenkette in der Programmiersprache C findet.

# Wie geht man vor?
Um die Länge einer Zeichenkette zu finden, müssen wir zunächst verstehen, wie Zeichenketten in C gespeichert werden. In C werden Zeichenketten als Arrays von einzelnen Zeichen dargestellt. Das bedeutet, dass wir die Länge der Zeichenkette anhand der Anzahl der gespeicherten Zeichen bestimmen können.

```C
char string[10] = "Hallo";
```

In diesem Beispiel haben wir eine Zeichenkette mit dem Inhalt "Hallo" erstellt. Die Länge dieser Zeichenkette ist 5, da sie 5 Zeichen enthält (inklusive des Null-Zeichens am Ende). Um die Länge einer Zeichenkette in C zu finden, können wir also einfach eine Schleife nutzen, die jedes Zeichen der Zeichenkette einzeln zählt.

```C
int string_laenge(char string[]) {
    int counter = 0;
    while (string[counter] != '\0') {
        counter++;
    }
    return counter;
}
```

Diese Funktion nimmt eine Zeichenkette als Parameter und zählt mithilfe einer while-Schleife jedes Zeichen, bis das Null-Zeichen erreicht wird. Das Ergebnis ist die Länge der Zeichenkette.

# Tiefer eintauchen
Es gibt auch andere Möglichkeiten, die Länge einer Zeichenkette in C zu finden. Zum Beispiel können wir die Funktion `strlen()` aus der Standardbibliothek `string.h` nutzen. Diese Funktion gibt ebenfalls die Länge einer Zeichenkette zurück und ist eine optimierte Version von unserer selbst geschriebenen Funktion.

```C
#include <string.h>

char string[10] = "Hallo";
int laenge = strlen(string); // laenge ist jetzt 5
```

Es ist auch wichtig zu beachten, dass bei der Verwendung von Zeichenketten in C immer genügend Speicherplatz für das Null-Zeichen eingeplant werden muss. Wenn wir versuchen, mehr Zeichen in eine Zeichenkette zu schreiben als der reservierte Speicher zulässt, kann es zu unerwarteten Fehlern im Programm kommen.

# Siehe auch
- [Die Funktion strlen() in der C-Dokumentation](https://www.tutorialspoint.com/c_standard_library/c_function_strlen.htm)
- [Arrays und Zeichenketten in C](https://www.programiz.com/c-programming/c-strings)