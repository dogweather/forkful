---
title:                "Entfernen von Zeichen, die einem Muster entsprechen"
html_title:           "C++: Entfernen von Zeichen, die einem Muster entsprechen"
simple_title:         "Entfernen von Zeichen, die einem Muster entsprechen"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Löschen von Zeichen, die zu einem bestimmten Muster passen, ist eine häufig verwendete Programmieraufgabe, bei der Zeichenfolgen manipuliert werden, um bestimmte Muster zu entfernen. Programmierer tun dies, um Texte zu bereinigen oder um Informationen zu extrahieren.

## Wie geht's?
Es gibt verschiedene Möglichkeiten, Zeichenfolgen in C++ zu manipulieren, um Zeichen zu löschen, die einem bestimmten Muster entsprechen. Einfacher ausgedrückt, lassen sich Zeichenfolgen mit der `erase()` Funktion bearbeiten. Hier ist ein Beispiel:
```C++
std::string text = "Hallo Welt";
text.erase(0, 6);
std::cout << text;
```
Das oben gezeigte Beispiel löscht die ersten sechs Zeichen in der Zeichenfolge `text` und gibt "Welt" aus. Der erste Parameter gibt den Index an, an dem das Löschen beginnen soll, und der zweite Parameter gibt die Anzahl der zu löschenden Zeichen an.

Es gibt auch die Möglichkeit, eine Schleife zu verwenden, um durch die Zeichenfolge zu iterieren und jedes Zeichen zu überprüfen, ob es dem gesuchten Muster entspricht. Wenn dies der Fall ist, kann das Zeichen gelöscht werden. Hier ist ein Beispiel:
```C++
std::string text = "Hallo Welt";
for (unsigned int i = 0; i < text.length(); i++) {
  if (text[i] == 'e') {
    text.erase(i, 1);
  }
}
std::cout << text;
```
Dieses Beispiel löscht alle "e"s in der Zeichenfolge `text` und gibt "Hallo Wlt" aus.

## Tief tauchen
Das Löschen von Zeichen in einer Zeichenfolge ist eine gängige Aufgabe in der Programmierung und es gibt viele Möglichkeiten, dies zu tun. Beispielsweise können Reguläre Ausdrücke verwendet werden, um komplexe Muster zu erkennen und zu löschen. Auch gibt es Funktionen wie `replace()` und `substr()`, die zur Manipulation von Zeichenfolgen verwendet werden können.

Bei der Implementierung ist es wichtig, darauf zu achten, dass die Anzahl der Zeichen in der Zeichenfolge nach dem Löschen möglicherweise nicht mehr mit dem ursprünglichen Wert übereinstimmt. Daher sollte die Länge der Zeichenfolge nach dem Löschen überprüft werden.

## Siehe auch
* [C++ Referenz - `erase()`](https://en.cppreference.com/w/cpp/string/basic_string/erase)
* [C++ Referenz - `replace()`](https://en.cppreference.com/w/cpp/string/basic_string/replace)
* [C++ Referenz - `substr()`](https://en.cppreference.com/w/cpp/string/basic_string/substr)
* [Reguläre Ausdrücke in C++](https://www.regular-expressions.info/cpp.html)