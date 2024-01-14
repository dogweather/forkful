---
title:                "C++: Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Manchmal kommt es vor, dass man in einer Zeichenkette bestimmte Zeichen löschen möchte, die einem bestimmten Muster entsprechen. Dies kann aus verschiedenen Gründen nützlich sein, wie zum Beispiel bei der Validierung von Benutzereingaben oder bei der Analyse von Daten.

## Wie geht man vor?

Um dieses Problem in C++ zu lösen, gibt es verschiedene Ansätze. Eine Möglichkeit ist die Verwendung der `std::string` Klasse und ihrer Methoden. Hier ein Beispiel mit dem Muster "abc", bei dem alle Vorkommnisse von "b" gelöscht werden:

```C++
#include <iostream>
#include <string>
 
int main() {
  std::string str = "abcdebbcdefb";
  std::string pattern = "b";
 
  // Schleife durch die Zeichenkette
  for (int i = 0; i < str.length(); i++) {
    // Überprüfen, ob das aktuelle Zeichen dem Muster entspricht
    if (str.substr(i, pattern.length()) == pattern) {
      // Falls ja, das Zeichen löschen
      str.erase(i, pattern.length());
    }
  }
 
  // Ausgabe der bearbeiteten Zeichenkette
  std::cout << str << std::endl;
 
  return 0;
}
```

Die Ausgabe dieses Beispiels wäre "acdecedef". Hier wurde jedes Vorkommnis von "b" gelöscht.

Natürlich gibt es auch noch andere Möglichkeiten, dieses Problem zu lösen, wie zum Beispiel die Verwendung von regulären Ausdrücken oder das Arbeiten mit Zeigern. Je nach Anwendungsfall kann die eine oder andere Methode besser geeignet sein.

## Eine tiefergehende Analyse

Wenn wir uns näher mit dem oben gezeigten Beispiel beschäftigen, gibt es ein paar Dinge, die man beachten muss. Zum einen werden die Zeichen von links nach rechts durchlaufen, was bedeutet, dass nur das erste Vorkommnis eines Musters gelöscht wird. Wenn wir also alle Vorkommnisse löschen wollen, müssen wir eine zusätzliche Schleife verwenden. Zum anderen könnten durch das Löschen die Indizes der Zeichen in der Zeichenkette verschoben werden. Dies könnte zu unerwünschten Ergebnissen führen oder sogar zu einem Programmabsturz führen. Daher ist es wichtig, die Indizes nach dem Löschen entsprechend anzupassen.

## Siehe auch

- Diese [Seite](https://www.cplusplus.com/reference/string/string/erase/) bietet weitere Informationen über die `erase()` Methode der `std::string` Klasse.
- Hier gibt es eine [Einführung](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm) in die Arbeit mit Zeichenketten in C++.
- Auf dieser [Seite](https://www.regular-expressions.info/) erfährt man mehr über reguläre Ausdrücke und wie man sie in C++ einsetzen kann.