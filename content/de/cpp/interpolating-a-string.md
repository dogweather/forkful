---
title:                "Eine Zeichenkette interpolieren"
html_title:           "Arduino: Eine Zeichenkette interpolieren"
simple_title:         "Eine Zeichenkette interpolieren"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die String-Interpolation in C++ ermöglicht es, Variablen innerhalb eines Strings zu ersetzen - das ist sehr nützlich, weil es den Code sauberer und einfacher zu lesen macht.

## So geht's:
```C++
#include <iostream>
#include <fmt/core.h> // for fmt lib

int main() {
    std::string name = "John";
    int age = 30;
    std::cout << fmt::format("Hello, {0}. You are {1} years old.", name, age);
    return 0;
}
```

Die Ausgabe wird sein:

```Output
Hello, John. You are 30 years old.
```

## Vertiefung:

1. Historischer Kontext: String Interpolation wird seit vielen Jahren in anderen Sprachen wie Perl und Python verwendet. C++ hingegen unterstützt diese Funktion nicht nativ und Erlaubt ihr durch fmt oder boost library.

2. Alternativen: Ein alternativer Weg ist der alte Stil der sprintf-Funktion in C. Aber das ist anfällig für Buffer Overflow und andere Probleme, daher ist es nicht empfohlen.

3. Implementierungsdetails: fmt library implementiert String Interpolation durch das Parsen von Eingabe-strings und dem Ersetzen von Platzhaltersymbolen mit den entsprechenden Werten während der Ausführungszeit.

## Weiterführende Links:
- fmt Library: https://github.com/fmtlib/fmt
- String-Interpolation in C++: https://www.geekhideout.com/printf.shtml
- Alternatives in C++: https://www.boost.org/doc/libs/1_77_0/libs/format/