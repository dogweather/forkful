---
title:                "C: Programmieren von Tests"
simple_title:         "Programmieren von Tests"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Viele Entwickler mögen es nicht, Tests zu schreiben. Es ist zusätzliche Arbeit und es scheint, als ob es Zeit verschwendet, die man stattdessen in das Schreiben von produktivem Code investieren könnte. Aber sobald man die Vorteile von Tests erkannt hat, wird man nie wieder ohne sie arbeiten wollen.

## Wie geht man vor?

Um effektiv Tests zu schreiben, muss man wissen, wie man sie schreibt. Am besten lernt man durch Beispiele, also werfen wir einen Blick auf einen einfachen Code-Block in C:

```C
#include <stdio.h>

int main(void) {
    int num1 = 5, num2 = 10;

    if (num1 < num2) {
        printf("num1 is smaller than num2.");
    }
    else {
        printf("num1 is greater than num2.");
    }
    return 0;
}
```

Dies ist ein sehr einfaches Beispiel, aber es zeigt, dass man einfach einen Test schreiben kann, indem man seine erwartete Ausgabe mit der tatsächlichen Ausgabe vergleicht. In diesem Fall erwarten wir, dass "num1 is smaller than num2." ausgegeben wird, da 5 kleiner als 10 ist. Wenn jedoch der Code geändert wird und "num1 is greater than num2." ausgegeben wird, wissen wir sofort, dass etwas falsch läuft.

Neben einfachen Vergleichen können Tests auch verwendet werden, um zu prüfen, ob bestimmte Funktionen richtig funktionieren. Zum Beispiel könnten wir eine Funktion implementieren, die zwei Zahlen multipliziert und dann einen Test schreiben, um sicherzustellen, dass sie die richtige Ausgabe zurückgibt.

## Tiefer in die Materie eintauchen

Natürlich gibt es viel mehr zu Tests als nur einfache Vergleiche. Je nach Sprache und Framework gibt es unterschiedliche Methoden und Techniken, um Tests zu schreiben. Es ist wichtig, sich eingehend mit den spezifischen Anforderungen des jeweiligen Projekts auseinanderzusetzen und die Tests entsprechend anzupassen.

Außerdem ist es wichtig, dass Tests regelmäßig ausgeführt werden, um sicherzustellen, dass sie immer noch korrekt sind. Wenn man Code ändert oder neue Funktionen hinzufügt, können Tests sehr nützlich sein, um potenzielle Probleme zu erkennen, bevor sie in der Produktion auftreten.

## Siehe auch

- [Einführung in das Testen von C-Code](https://www.freecodecamp.org/news/test-driven-development-what-it-is-and-how-to-use-it/)
- [CUnit: Ein Framework für das Testen von C-Code](https://cunit.sourceforge.io/)
- [Warum Tests für Entwickler wichtig sind](https://www.thoughtworks.com/insights/blog/unit-test)