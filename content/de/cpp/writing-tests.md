---
title:                "Tests schreiben"
html_title:           "C++: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/writing-tests.md"
---

{{< edit_this_page >}}

Was & Warum?

Tests schreiben ist ein wesentlicher Bestandteil der Programmierung. Es bezieht sich auf das Schreiben von Code, der die Funktionalität und Korrektheit anderer Codefragmente prüft. Programmer schreiben Tests, um sicherzustellen, dass ihr Code zuverlässig funktioniert und mögliche Fehler identifiziert werden können.

Wie geht's?

### Einfaches Beispiel

```C++
#include <iostream>
using namespace std;

int main() {
  int a = 5;
  int b = 10;

  if (a + b == 15) {
    cout << "Das Ergebnis ist richtig!" << endl;
  } else {
    cout << "Das Ergebnis ist falsch!" << endl;
  }
}
```

Ausgabe: Das Ergebnis ist richtig!

### Komplexerer Beispiel

```C++
#include <algorithm>
#include <cassert>
#include <vector>
using namespace std;

// Funktion zum Sortieren von Vektoren
vector<int> sortVector(vector<int>& v) {
  sort(v.begin(), v.end());
  return v;
}

int main() {
  // Vektor erstellen und Werte hinzufügen
  vector<int> v{5, 3, 9, 6, 2};

  // Erwartete Ergebnisse
  vector<int> expected{2, 3, 5, 6, 9};

  // Das Ergebnis der Sortierfunktion
  vector<int> result = sortVector(v);

  // Überprüfen, ob das Ergebnis richtig sortiert ist
  assert(result == expected);
}
```

### Ausgabe:

Keine Ausgabe, da das Programm ohne Fehler ausgeführt wurde.

Tiefere Einblicke

Tests schreiben ist keine neue Konzeption in der Programmierung. In der Vergangenheit haben sich Entwickler auf manuelles Testen verlassen, um ihre Codes zu überprüfen. Mit dem Wachstum der Software-Industrie und der Einführung von agilen Methoden wurde es jedoch unpraktisch und zeitaufwendig, alle Codeänderungen manuell zu testen. Daher wurden automatisierte Tests eingeführt, die effizienter sind und genaue Ergebnisse liefern.

Eine Alternative zum Schreiben von Tests ist das sogenannte "Test-Driven Development" (TDD). Dabei schreibt der Entwickler zunächst den Test und schreibt dann den entsprechenden Code, um den Test zu bestehen. Dieser Ansatz wird als effektiv angesehen, da er den Fokus auf die Funktionalität und Korrektheit des Codes legt.

Ein wichtiger Aspekt beim Schreiben von Tests ist die Wahl der richtigen Testfälle. Es ist wichtig, sowohl positive als auch negative Testfälle abzudecken, um sicherzustellen, dass der Code in allen möglichen Szenarien richtig funktioniert. Eine gute Testabdeckung sorgt für mehr Vertrauen in den Code und erleichtert die Wartung und Erweiterung im späteren Verlauf.

Sieh dir auch an

- [Einführung in das Testen von C++-Code](https://www.tutorialspoint.com/cplusplus/cpp_unit_testing.htm)
- [Testen mit Google Test in C++](https://www.inf.hs-flensburg.de/lang/cplusplus/CppUnit.html)
- [Test-Driven Development mit C++](https://www.softwaretestinghelp.com/test-driven-development-with-cpp-tdd/)