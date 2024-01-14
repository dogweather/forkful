---
title:                "C++: Debug-Ausgabe drucken"
programming_language: "C++"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Debugging ist eine wichtige Methode, um Fehler in unserem Code zu finden und zu beheben. Das Drucken von Debug-Ausgaben ist ein hilfreiches Werkzeug, um uns bei diesem Prozess zu unterstützen, indem es uns Einblicke in den aktuellen Zustand unserer Programme gibt.

## Wie man Debug-Ausgaben druckt

Es gibt verschiedene Möglichkeiten, Debug-Ausgaben in C++ zu drucken. Eine einfache Methode ist die Verwendung von `std::cout` und `std::cerr`, um Informationen auf der Konsole auszugeben. Hier ist ein Beispiel:

```C++
#include <iostream>

int main() {
  int number = 5;
  std::cout << "Number is: " << number << std::endl;  // 1. Debug-Ausgabe
  std::cerr << "Something went wrong!" << std::endl;  // 2. Debug-Ausgabe
  return 0;
}
```

Die Ausgabe dieses Codes wäre:

```
Number is: 5
Something went wrong!
```

Die erste Debug-Ausgabe, die mit `std::cout` gedruckt wurde, gibt uns Informationen darüber, was der Wert der Variable `number` ist. Die zweite Ausgabe, die mit `std::cerr` gedruckt wurde, zeigt uns an, dass im Code etwas schief gelaufen ist.

Eine andere Möglichkeit, Debug-Ausgaben zu drucken, ist die Verwendung von `assert`-Statements. Diese prüfen bestimmte Bedingungen und geben eine Fehlermeldung aus, wenn diese Bedingungen nicht erfüllt sind. Ein Beispiel hierfür wäre:

```C++
#include <cassert>

int main() {
  int number = 5;
  assert(number == 10);  // 3. Debug-Ausgabe
  return 0;
}
```

Wenn die Bedingung im `assert`-Statement nicht erfüllt ist, wird eine Fehlermeldung ausgegeben, die uns mitteilt, was schief gelaufen ist.

Es ist auch möglich, mit Visual Studio und anderen IDEs Schritt-für-Schritt-Debugging zu verwenden, um den genauen Verlauf des Programms zu verfolgen und den Wert von Variablen zu überwachen.

## Deep Dive

Das Drucken von Debug-Ausgaben kann uns nicht nur helfen, Fehler zu finden und zu beheben, sondern auch dabei, den Ablauf des Programms besser zu verstehen. Es kann auch bei der Performance-Optimierung von Code hilfreich sein, indem es uns zeigt, welche Abschnitte des Programms am längsten dauern und wo mögliche Engpässe liegen könnten.

Es gibt jedoch einige Dinge, die wir beim Drucken von Debug-Ausgaben beachten sollten. Zunächst einmal sollten wir sicherstellen, dass diese Ausgaben nur in der Entwicklungsphase verwendet werden und nicht in der Produktionsumgebung. Debug-Ausgaben können die Geschwindigkeit des Programms beeinträchtigen und sollten daher nur für den Test- und Debugging-Zweck verwendet werden.

Wir sollten auch darauf achten, nicht zu viele Debug-Ausgaben zu drucken, da dies den Code unübersichtlich machen und seine Lesbarkeit beeinträchtigen könnte. Es ist wichtig, nur die relevanten Informationen auszugeben, die uns bei der Fehlerbehebung oder Optimierung helfen.

## Siehe auch

- [C++ Debugging Tutorial (Englisch)](https://www.geeksforgeeks.org/c-plus-plus/)
- [Schritt-für-Schritt-Debugging in Visual Studio (Englisch)](https://docs.microsoft.com/en-us/visualstudio/debugger/getting-started-with-the-debugger-cpp?view=vs-2019)
- [Debugging Tips für C++ (Englisch)](https://docs.microsoft.com/en-us/cpp/cpp/tips-for-debugging-cpp-code?view=vs-2019)