---
title:                "Bitte keine Kommentare hinzufügen.String großschreiben"
html_title:           "C++: Bitte keine Kommentare hinzufügen.String großschreiben"
simple_title:         "Bitte keine Kommentare hinzufügen.String großschreiben"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Was ist das und Warum?

Beim "Capitalizing" wird der erste Buchstabe eines Strings in Großbuchstaben umgewandelt. Programmierer nutzen dies, um Strings besser lesbar zu machen und ein einheitliches Format zu gewährleisten.

# How to:

Um einen String zu capitalizen, gibt es mehrere Möglichkeiten in C++. Eine einfache Möglichkeit ist die Verwendung der Funktion `toupper()` aus der Bibliothek `<cctype>`. Der folgende Code block zeigt, wie dies in einem Beispiel funktioniert:

```C++
#include <iostream>
#include <string>
#include <cctype>

int main() {
    std::string str = "Hallo Welt";
    str[0] = toupper(str[0]);
    std::cout << str << std::endl;
    return 0;
}
```

Der output dieses Codes wäre "Hallo Welt", da der erste Buchstabe "H" zu "h" geändert wurde.

Eine andere Möglichkeit ist die Verwendung der `transform()` Funktion aus der Bibliothek `<algorithm>` zusammen mit einer Lambda-Funktion. Der folgende Code zeigt ein Beispiel hierfür:

```C++
#include <iostream>
#include <string>
#include <algorithm>

int main() {
    std::string str = "Hallo Welt";
    std::transform(str.begin(), str.end(), str.begin(), [](unsigned char c){ return std::toupper(c); });
    std::cout << str << std::endl;
    return 0;
}
```

Durch die Verwendung von `transform()` und der Lambda-Funktion wird jeder einzelne Buchstabe des Strings in einen Großbuchstaben umgewandelt.

# Deep Dive

Das Konzept des "Capitalizing" stammt aus der Programmiersprache COBOL und wurde in den 1950er Jahren eingeführt. In C++ gibt es mehrere Möglichkeiten, einen String zu verändern. Neben dem "Capitalizing" gibt es auch noch die Funktionen `tolower()` und `isupper()` aus der Bibliothek `<cctype>`.

Wenn man einheitliche Formatierungen in einem Programm gewährleisten möchte, ist das "Capitalizing" von Strings hilfreich.

# See Also

- [`toupper()` reference](https://www.cplusplus.com/reference/cctype/toupper/)
- [`transform()` reference](https://www.cplusplus.com/reference/algorithm/transform/)
- [History of string capitalization](https://en.wikipedia.org/wiki/Letter_case#The_history_of_letter_case)