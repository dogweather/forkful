---
title:                "Tests schreiben"
html_title:           "C: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/writing-tests.md"
---

{{< edit_this_page >}}

## Was & Warum?

"Tests schreiben" ist eine Methode, die von Programmierern verwendet wird, um zu überprüfen ob der Code funktioniert wie erwartet. Bei der Entwicklung von Software können unerwartete Fehler auftreten, und Tests helfen dabei, diese Fehler frühzeitig zu erkennen und zu beheben.

## Wie geht man vor?

Um Tests für C-Code zu schreiben, kann die Standardbibliothek "assert.h" verwendet werden. Diese bietet Funktionen wie "assert()" und "static_assert()", die es ermöglichen, Bedingungen zu überprüfen und gegebenenfalls Fehlermeldungen auszugeben. Hier ist ein einfaches Beispiel:

```C
#include <assert.h>
#include <stdio.h>

int sum(int a, int b) {
  return a + b;
}

int main() {
  int result = sum(2, 3);
  
  assert(result == 5); // Überprüfe ob die Funktion "sum" das richtige Ergebnis liefert
  
  printf("Test erfolgreich!"); // Wird nur bei erfolgreichem Test ausgegeben
  
  return 0;
}
```

Ausgabe:

```
Test erfolgreich!
```

## Tiefere Einblicke

Das Schreiben von Tests ist eine bewährte Methode, die bereits seit langer Zeit in der Softwareentwicklung verwendet wird. Durch das Testen kann man sicherstellen, dass der Code robust und fehlerfrei ist. Es gibt auch alternative Ansätze wie das Test-Driven-Development, bei dem Tests zuerst geschrieben werden und dann der Code entsprechend angepasst wird.

Um Tests effektiv zu schreiben, sollte man sich mit den verschiedenen Testarten, wie z.B. Unit-Tests und Integrationstests, vertraut machen. Außerdem ist es wichtig, Testabdeckung und Code Reviews zu nutzen, um eine hohe Qualität des Codes zu gewährleisten.

## Siehe auch

- [Unit Testing in C with Check](https://libcheck.github.io/check/)
- [Test-Driven-Development in C](http://c.learncodethehardway.org/book/ex47.html)