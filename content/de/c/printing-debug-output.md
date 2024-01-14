---
title:                "C: Ausgabe von Debug-Informationen"
programming_language: "C"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Das Debuggen von Code ist für jeden Programmierer ein wichtiger Schritt bei der Entwicklung von Software. Eine Möglichkeit, um Probleme im Code zu identifizieren und zu beheben, ist die Verwendung von Debug-Ausgaben. In diesem Artikel werden wir uns genauer anschauen, wie man Debug-Ausgaben in C-Programmen verwendet.

## Wie man Debug-Ausgaben in C verwendet

Debug-Ausgaben können auf verschiedene Arten in ein C-Programm eingefügt werden. Eine Möglichkeit ist die Verwendung der "printf()" Funktion. Diese Funktion ermöglicht es uns, Text auf der Konsole auszugeben, um bestimmte Variablenwerte oder Nachrichten während des Programmablaufs anzuzeigen.

```C
#include <stdio.h>

int main() {
  int num = 5;
  printf("Der Wert von num ist %d", num);
  return 0;
}
```

Die verwendete Formatierung "%d" gibt an, dass die Ausgabe eine Zahl sein wird. Alternativ können wir auch den Variablennamen direkt in den String einfügen.

```C
#include <stdio.h>

int main() {
  int num = 5;
  printf("Der Wert von num ist %d", num);
  return 0;
}
```

In diesem Beispiel haben wir die Variable "num" ohne Angabe der Formatierung direkt in den String eingefügt. Beachte, dass wir die Variablen innerhalb der Funktion "printf()" nutzen können, auch wenn sie außerhalb der Funktion deklariert wurden.

## Tiefere Einblicke

Debug-Ausgaben können auch verwendet werden, um den Ablauf des Programms nachzuvollziehen. Mit der Funktion "printf()" können wir auch in Schleifen oder Bedingungen Debug-Ausgaben einfügen, um zu überprüfen, ob der Code so ausgeführt wird, wie wir es erwarten.

```C
#include <stdio.h>

int main() {
  int i;
  for(i = 0; i < 10; i++) {
    printf("Der Wert von i ist %d", i);
  }
  return 0;
}
```

In diesem Beispiel geben wir den Wert von "i" in jeder Iteration der Schleife aus, um zu sehen, ob er wie erwartet von 0 bis 9 inkrementiert wird. Auf diese Weise können wir mögliche Fehler oder unerwartete Ergebnisse leichter erkennen.

## Siehe auch

- [Debugging mit GDB](https://www.gnu.org/software/gdb/): Ein mächtiges Debugging-Tool für C-Programme.
- [Debugging-Tipps für C-Programme](https://c.learncodethehardway.org/book/ex20.html): Ein Artikel mit hilfreichen Tipps zum Debuggen von C-Code.