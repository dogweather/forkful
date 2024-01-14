---
title:    "C: Vergleich von zwei Datumsangaben"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Datumswerten ist eine häufige Aufgabe in der Programmierung. Es ermöglicht uns, festzustellen, ob ein bestimmtes Ereignis vor oder nach einem anderen stattgefunden hat. In diesem Blog-Post werden wir uns damit beschäftigen, wie man in der Programmiersprache C zwei Daten vergleichen kann.

## Wie

Um zwei Daten in C zu vergleichen, können wir den Operator "==" verwenden. Dieser Operator vergleicht die Werte links und rechts von ihm und gibt true zurück, wenn sie gleich sind, und false, wenn sie unterschiedlich sind.

```C
#include <stdio.h>

int main() {
    // Datumswerte, die verglichen werden sollen
    int datum1 = 20200101;
    int datum2 = 20210101;

    // Vergleich mit dem "==" Operator
    if (datum1 == datum2) {
        printf("Die beiden Daten sind gleich.\n");
    } else {
        printf("Die beiden Daten sind unterschiedlich.\n");
    }

    return 0;
}

```

Dieses Beispiel vergleicht zwei Datenwerte und gibt entsprechend eine Nachricht aus. In diesem Fall sind die Daten unterschiedlich, daher wird die Meldung "Die beiden Daten sind unterschiedlich." ausgegeben.

## Deep Dive

Es gibt jedoch einige wichtige Dinge zu beachten, wenn man Daten in C vergleicht. Zum einen müssen beide Werte vom gleichen Typ sein, sonst kann es zu unerwarteten Ergebnissen kommen. Zum anderen ist der "==" Operator in C nicht genau genug für einige Anwendungsfälle, wie zum Beispiel das Vergleichen von Uhrzeiten. In diesen Fällen muss man auf spezielle Funktionen zurückgreifen, die in der C-Bibliothek zur Verfügung stehen.

## Siehe auch

Hier sind einige hilfreiche Ressourcen, die weitere Informationen zum Vergleichen von Daten in C bieten:

- [C-Programmierung Tutorial](https://www.learn-c.org/)
- [C-Dokumentation](https://devdocs.io/c/)
- [Vergleichen von Daten in C - GeeksforGeeks](https://www.geeksforgeeks.org/time-complexity-comparisons-of-c-different-methods-to-check-two-strings-are-anagram-or-not/)