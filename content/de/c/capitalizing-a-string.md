---
title:    "C: String in Großbuchstaben umwandeln"
keywords: ["C"]
---

{{< edit_this_page >}}

## Warum

Das Verschachteln von Strings ist eine grundlegende Aufgabe in der Programmierung. Es kann hilfreich sein, um sicherzustellen, dass Benutzereingaben oder Daten in einer bestimmten Form vorliegen und leichter verarbeitet werden können. Insbesondere in der Programmiersprache C ist dies nützlich, da sie keine integrierten Funktionen zur String-Manipulation hat.

## So geht's

Um eine Zeichenfolge in C zu kapitalisieren, muss jeder einzelne Buchstabe in einen Großbuchstaben umgewandelt werden. Dazu kann die Funktion `toupper()` aus der Bibliothek `<ctype.h>` verwendet werden. Im folgenden Beispiel wird eine Benutzereingabe eingelesen und dann in Großbuchstaben umgewandelt:

```C
#include <stdio.h>
#include <ctype.h>

int main() {
    // Benutzereingabe einlesen
    char input[50];
    printf("Gib einen Text ein: ");
    scanf("%s", input);

    // Schleife durchläuft den eingegebenen Text
    for (int i = 0; input[i] != '\0'; i++) {
        // Wandle jeden Buchstaben in einen Großbuchstaben um
        input[i] = toupper(input[i]);
    }

    // Ausgabe des kapitalisierten Strings
    printf("Deine Eingabe in Großbuchstaben: %s", input);

    return 0;
}
```

Beispiel-Ausgabe:
```
Gib einen Text ein: Hallo Welt
Deine Eingabe in Großbuchstaben: HALLO WELT
```

## Tiefer Einblick

In der C-Standardbibliothek sind viele nützliche Funktionen für die Zeichen- und String-Manipulation enthalten. `toupper()` ist eine davon und wird verwendet, um einen einzelnen Buchstaben in einen Großbuchstaben umzuwandeln. Diese Funktion ist in der Header-Datei `<ctype.h>` deklariert und nimmt einen `int` als Argument, der die ASCII-Nummer eines Zeichens darstellt. Sie gibt dann eine `int` zurück, die den entsprechenden Großbuchstaben repräsentiert.

Es ist auch möglich, eigene Funktionen zu schreiben, die Zeichenfolgen in Großbuchstaben umwandeln. Dies kann durch eine einfache Schleife und die Verwendung von arithmetischen Operatoren erreicht werden. Zum Beispiel kann die ASCII-Nummer des kleinen Buchstabens 'a' (`97`) um 32 erhöht werden, um die entsprechende Großbuchstaben-Nummer `65` zu erhalten.

## Siehe auch

- [String-Manipulation in C – Das Wichtigste auf einen Blick](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [Übersicht der Funktionen in <ctype.h>](https://en.cppreference.com/w/c/string/byte/toupper)