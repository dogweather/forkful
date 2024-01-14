---
title:                "C++: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum
Es gibt viele Situationen, in denen man die Länge einer Zeichenkette in einem C++ Programm bestimmen möchte. Zum Beispiel kann es nützlich sein, um die genaue Größe einer Datei zu ermitteln oder um sicherzustellen, dass eine Eingabe nicht zu lang ist.

## Wie geht man vor
Um die Länge einer Zeichenkette in C++ zu finden, gibt es verschiedene Methoden. Eine Möglichkeit ist die Verwendung der `strlen()` Funktion aus der Standard C Bibliothek. Diese Funktion akzeptiert eine Zeichenkette als Argument und gibt die Anzahl der Buchstaben in der Zeichenkette zurück. Ein Beispielcode sieht wie folgt aus:
```C++
#include <iostream>
#include <cstring>

using namespace std;

int main() {
    char string[] = "Hallo Welt";
    int length = strlen(string);
    cout << "Die Länge des Strings ist: " << length << endl;
    return 0;
}
```
Die Ausgabe des obigen Codes wäre: `Die Länge des Strings ist: 10`.

Eine weitere Methode ist die Verwendung der `size()` Funktion der `std::string` Klasse. Diese Methode gibt ebenfalls die Länge einer Zeichenkette zurück und kann bei der Verwendung von `std::string` Objekten nützlich sein. Ein Beispielcode hierfür wäre:
```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string string = "Hallo Welt";
    int length = string.size();
    cout << "Die Länge des Strings ist: " << length << endl;
    return 0;
}
```
Die Ausgabe dieses Codes wäre ebenfalls: `Die Länge des Strings ist: 10`.

Beide Methoden sind gültig und es ist eine Frage der persönlichen Präferenz und der jeweiligen Situation, welche verwendet wird.

## Tiefergehende Informationen
Beide Methoden, `strlen()` und `size()`, geben die Anzahl der Buchstaben in einer Zeichenkette zurück. Dabei werden alle Zeichen, einschließlich Leerzeichen und Sonderzeichen, mitgezählt. Es ist wichtig zu beachten, dass die Terminierung mit dem Nullzeichen bei der Längenbestimmung ebenfalls berücksichtigt wird.

Um die genaue Länge einer Zeichenkette zu bestimmen, kann es sinnvoll sein, auch die Leerzeichen, Sonderzeichen und das Nullzeichen zu berücksichtigen. In diesem Fall muss die Länge entsprechend angepasst werden, indem man die Anzahl der Leerzeichen und Sonderzeichen addiert und das Nullzeichen abzieht.

## Siehe auch
- [C++ Referenz: strlen()](https://en.cppreference.com/w/cpp/string/byte/strlen)
- [C++ Referenz: size()](https://en.cppreference.com/w/cpp/string/basic_string/size)