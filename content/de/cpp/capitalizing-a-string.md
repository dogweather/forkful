---
title:                "C++: Ein String großschreiben"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Verschriftlichen von Wörtern ist eine grundlegende Funktion in der Programmierung. Es kann jedoch vorkommen, dass ein String in Kleinbuchstaben geschrieben ist und für die Ausgabe oder die Verwendung in einer anderen Funktion in Großbuchstaben umgewandelt werden muss. Deshalb ist das "Kapitalisieren" eines Strings ein nützliches Werkzeug in der C++ Programmierung.

## Wie geht das?

Das Kapitalisieren eines Strings in C++ kann auf verschiedene Weisen erreicht werden. Eine Möglichkeit ist die Verwendung der "transform" Funktion aus der STL Bibliothek, wie im folgenden Beispiel gezeigt:

```C++
#include <iostream>
#include <algorithm>
#include <string>

int main() {
    std::string text = "hallo welt";
    // Umwandlung in Großbuchstaben
    std::transform(text.begin(), text.end(), text.begin(), toupper);
    std::cout << text << std::endl;
    // Ausgabe: HALLO WELT
    return 0;
}
```

Eine weitere Möglichkeit ist die Verwendung des ASCII Codes, um zwischen Klein- und Großbuchstaben zu unterscheiden. Dies kann mit einem einfachen Loop und der Verwendung von "islower" und "toupper" Funktionen erreicht werden, wie im folgenden Beispiel gezeigt:

```C++
#include <iostream>
#include <string>

int main() {
    std::string text = "hallo welt";
    // Loop durch jeden Buchstaben im String
    for (int i = 0; i < text.length(); i++) {
        // Überprüfen, ob der Buchstabe ein Kleinbuchstabe ist
        if(islower(text[i])) {
            // Konvertierung in Großbuchstabe
            text[i] = toupper(text[i]);
        }
    }
    std::cout << text << std::endl;
    // Ausgabe: HALLO WELT
    return 0;
}
```

Es gibt noch viele weitere Möglichkeiten, einen String in C++ zu kapitalisieren, daher empfehle ich, verschiedene Methoden auszuprobieren und die für dich am besten geeignete zu verwenden.

## Tiefergehender Einblick

Die "transform" Funktion und die Verwendung von ASCII Codes sind nur zwei Beispiele für Möglichkeiten, Strings in C++ zu kapitalisieren. Es gibt viele weitere Methoden, die sich in Effizienz und Genauigkeit der Umwandlung unterscheiden. Es ist wichtig, die verschiedenen Möglichkeiten zu kennen und die beste für deine spezielle Situation auszuwählen.

## Siehe auch

- [C++ String Manipulation Funktionen](http://www.cplusplus.com/reference/string/string/)
- [ASCII Tabelle](http://www.asciitable.com/)
- [C++ Touppper Funktion](http://www.cplusplus.com/reference/cctype/toupper/)
- [C++ Transform Funktion](http://www.cplusplus.com/reference/algorithm/transform/)