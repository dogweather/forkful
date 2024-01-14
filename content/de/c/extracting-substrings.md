---
title:                "C: Extrahieren von Teilstrings"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/extracting-substrings.md"
---

{{< edit_this_page >}}

# Warum
Substring-Extraktion ist eine nützliche Funktion in der Programmierung, die es ermöglicht, bestimmte Teile eines Textes zu extrahieren, anstatt den gesamten Text zu bearbeiten. Dies kann zeitsparend sein und die Effizienz des Codes erhöhen. In diesem Blog-Beitrag werden wir uns damit beschäftigen, wie man Substrings in C extrahiert und tiefer in dieses Konzept eintaucht.

# Wie man Substrings in C extrahiert
Um Substrings in C zu extrahieren, können wir die Funktion `strncpy` verwenden, die Teil der `string.h` Bibliothek ist. Diese Funktion wird verwendet, um einen Teil eines Strings in einen anderen String zu kopieren. Der Syntax für die Verwendung von `strncpy` ist wie folgt:

```C
strncpy(Zielstring, Ausgangsstring, Anzahl_der_Zeichen)
```
Dabei ist `Zielstring` der String, in den der Substring kopiert werden soll, `Ausgangsstring` ist der ursprüngliche String, aus dem der Substring extrahiert wird und `Anzahl_der_Zeichen` gibt an, wie viele Zeichen kopiert werden sollen.

Um dies besser zu verstehen, werfen wir einen Blick auf ein Beispiel:

```C
#include <stdio.h>
#include <string.h>

int main()
{
    char ursprungsstring[] = "Willkommen";
    char zielstring[5];

    strncpy(zielstring, ursprungsstring, 5);

    printf("Der Substring ist: %s", zielstring);

    return 0;
}
```

In diesem Beispiel haben wir den ursprünglichen String "Willkommen" und kopieren die ersten 5 Zeichen in den Zielstring. Ausgegeben wird der Substring "Willk".

# Tiefer Einblick in die Substring-Extraktion
Neben `strncpy` gibt es auch andere Funktionen, die für die Substring-Extraktion verwendet werden können, wie zum Beispiel `strncat` und `strndup`. Diese Funktionen haben jedoch unterschiedliche Syntax und Funktionsweise.

Ein weiteres wichtiges Konzept bei der Substring-Extraktion ist das Indexing. In C beginnt das Indexing bei 0, was bedeutet, dass das erste Zeichen eines Strings den Index 0 hat. Um einen bestimmten Substring zu extrahieren, müssen wir daher wissen, an welcher Stelle im String er beginnt und wo er endet.

Es ist auch wichtig zu beachten, dass bei Verwendung der Funktion `strncpy` der Zielstring immer länger sein muss als der zu kopierende Substring. Andernfalls kann es zu Speicherzugriffsverletzungen und Fehlern im Code kommen.

# Siehe auch
- [C String Operations](https://www.tutorialspoint.com/cprogramming/c_string.htm)
- [C Strncpy Function](https://www.w3schools.in/c-tutorial/string-handling/strncpy-function/)
- [C Examples - String handling functions](https://www.programiz.com/c-programming/examples/string-handling-functions)