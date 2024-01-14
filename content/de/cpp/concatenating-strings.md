---
title:    "C++: Zeichenketten verbinden"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

In der Welt der Programmierung gibt es immer wieder Situationen, in denen wir Daten miteinander kombinieren möchten. Eine gängige Methode, um dies in C++ zu tun, ist die Verkettung von Zeichenketten oder auch "Strings" genannt. In diesem Blog-Beitrag werden wir uns genauer ansehen, warum die Verkettung von Strings eine wichtige Fähigkeit für jeden Programmierer ist.

## Wie geht man vor

Um Strings zu verketten, gibt es verschiedene Möglichkeiten in C++. Die einfachste Methode ist die Verwendung des `+` Operators. Hier ist ein Beispiel:

```C++
#include <iostream>
#include <string>

int main() {
    // Erstelle zwei Strings
    std::string name = "Max";
    std::string greeting = "Hallo ";

    // Verkette die beiden Strings
    std::string message = greeting + name;

    // Gib die verketteten Strings aus
    std::cout << message << std::endl;

    return 0;
}

// Output: Hallo Max
```

Wie du sehen kannst, können wir einfach den `+` Operator verwenden, um zwei Strings miteinander zu verketten. Beachte jedoch, dass beide Strings vom gleichen Datentyp sein müssen (in diesem Fall beide `std::string`). Außerdem können wir mehr als zwei Strings auf diese Weise verketten, indem wir einfach mehrere `+` Operatoren hintereinander verwenden.

Es gibt auch noch eine andere Möglichkeit, Strings zu verketten, indem wir die `append()` Funktion verwenden. Hier ist ein Beispiel:

```C++
#include <iostream>
#include <string>

int main() {
    // Erstelle zwei Strings
    std::string name = "Max";
    std::string greeting = "Hallo ";

    // Verkette die beiden Strings
    greeting.append(name);

    // Gib die verketteten Strings aus
    std::cout << greeting << std::endl;

    return 0;
}

// Output: Hallo Max
```

Dieses Beispiel macht im Grunde dasselbe wie das vorherige, jedoch wird hier die `append()` Funktion verwendet, um den String `greeting` um den String `name` zu erweitern.

## Tieferer Einblick

Der `+` Operator und die `append()` Funktion sind nicht die einzigen Möglichkeiten, um Strings zu verketten. Es gibt noch viele andere nützliche Funktionen in C++, die uns dabei helfen können, komplexe Verkettungen von Strings zu erstellen. Zum Beispiel gibt es die `insert()` Funktion, um einen String in einen anderen einzufügen oder auch die `replace()` Funktion, um einen Teil eines Strings durch einen anderen zu ersetzen.

Ein weiterer wichtiger Aspekt der String-Verkettung ist die Verwendung von Escape-Sequenzen. Diese Zeichenfolgen, die mit einem `\` beginnen, ermöglichen es uns, spezielle Zeichen in einen String einzufügen, wie z.B. einen Zeilenumbruch (`\n`) oder eine Tabulator-Position (`\t`). Ohne Escape-Sequenzen wäre es unmöglich, solche Zeichen innerhalb eines Strings zu verwenden.

In C++ gibt es auch die Möglichkeit, Strings von verschiedenen Datentypen zu verketten, indem man sogenannte "Stringstreams" verwendet. Diese erlauben es uns, Daten von verschiedenen Typen miteinander zu kombinieren und dann in einen String zu "streamen".

## Siehe auch

- [C++ String Concatenation](https://www.geeksforgeeks.org/concatenate-strings-in-c-using-operator-append-stringstream/)
- [C++ Escape Sequences](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)