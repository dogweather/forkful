---
title:                "Die Länge eines Strings berechnen"
html_title:           "C++: Die Länge eines Strings berechnen"
simple_title:         "Die Länge eines Strings berechnen"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Die Länge einer Zeichenkette zu finden ist ein häufiges Problem bei der Programmierung. Es bezieht sich auf die Anzahl der Zeichen in einer Zeichenkette und ist wichtig, um mit dem Inhalt von Zeichenketten zu arbeiten. Da Zeichenketten in der Programmierung häufig verwendet werden, ist es wichtig zu wissen, wie man ihre Länge bestimmen kann.

## How to:

Verwenden Sie die built-in-Funktion ```C++ size()``` um die Länge einer Zeichenkette zu erhalten:

```C++
#include <iostream>
#include <string> 

using namespace std;

int main() {
    string str = "Hallo World!";
    cout << "Die Länge der Zeichenkette ist: " << str.size() << endl;
    return 0;
}
```
Output: Die Länge der Zeichenkette ist: 12

Sie können auch eine Schleife verwenden, um die Anzahl der Zeichen in einer Zeichenkette zu zählen:

```C++
#include <iostream>

using namespace std;

int main() {
    string str = "Hallo World!";
    int count = 0;
    while (str[count] != '\0') {
        count++;
    }
    cout << "Die Länge der Zeichenkette ist: " << count << endl;
    return 0;
}
```

Output: Die Länge der Zeichenkette ist: 12

## Deep Dive:

Die Funktion ```C++ size()``` ist Teil der Standard Template Library (STL) und wurde in der Version C++98 eingeführt. Sie wird verwendet, um die Größe oder die Länge einer Datenstruktur zu ermitteln, einschließlich Zeichenketten. Alternativ können Sie auch die Funktion ```C++ strlen()``` verwenden, um die Länge einer Zeichenkette zu erhalten. Diese Funktion ist jedoch etwas weniger effizient als ```C++ size()```.

Die Länge einer Zeichenkette kann auch durch die Verwendung von Pointern und Schleifen ermittelt werden. Dies ist eine grundlegende Methode, die jedoch in der Praxis nicht empfohlen wird, da sie anfällig für Fehler ist und nicht so effizient wie die Verwendung von built-in-Funktionen. 

## See Also:

Weitere Informationen zur Funktion ```C++ size()``` finden Sie in der offiziellen C++ Dokumentation: https://en.cppreference.com/w/cpp/string/basic_string/size

Weitere Möglichkeiten, um die Länge einer Zeichenkette in C++ zu ermitteln, finden Sie in diesem Tutorial: https://www.geeksforgeeks.org/length-of-a-string-using-pointers/