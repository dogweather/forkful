---
title:                "Verbinden von Zeichenketten"
html_title:           "C++: Verbinden von Zeichenketten"
simple_title:         "Verbinden von Zeichenketten"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Wenn du in deiner C++ Programmierung mit Texten arbeitest, wirst du früher oder später auf die Notwendigkeit stoßen, Strings (Zeichenketten) zu verbinden. Das können zum Beispiel Wörter oder Sätze sein, die du zusammensetzen möchtest. In diesem Artikel zeige ich dir, wie du das auf einfache Art und Weise in C++ machen kannst.

## Wie

Die einfachste Methode, Strings in C++ zu verbinden, ist die Verwendung des "+" Operators. Hier ein Beispiel:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string first_name = "Anna";
    string last_name = "Müller";

    string full_name = first_name + " " + last_name;
    cout << full_name; // Ausgabe: Anna Müller

    return 0;
}
```

Wie du sehen kannst, müssen die Strings, die du verbinden möchtest, mit dem "+" Operator und dem entsprechenden Leerzeichen dazwischen geschrieben werden. Du kannst beliebig viele Strings auf diese Weise miteinander verbinden.

Es ist auch möglich, Strings mit Zahlen oder anderen Variablen zu verbinden. Hier ein weiteres Beispiel:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string fruit = "Apfel";
    int number = 5;

    string sentence = "Ich habe " + to_string(number) + " " + fruit + "en gegessen!";
    cout << sentence; // Ausgabe: Ich habe 5 Äpfel gegessen!

    return 0;
}
```

In diesem Beispiel wird die Funktion "to_string()" verwendet, um die Zahl in einen String umzuwandeln.

## Deep Dive

In C++ gibt es auch die Möglichkeit, Strings mit der Funktion "concat()" aus der Bibliothek <cstring> zu verbinden. Es ist jedoch wichtig zu wissen, dass der "concat()" Befehl den ursprünglichen String verändert. Falls du den ursprünglichen String behalten möchtest, musst du ihn zuerst in einen neuen String kopieren. Hier ein Beispiel:

```C++
#include <iostream>
#include <string>
#include <cstring>

using namespace std;

int main() {
    char string1[20] = "Hallo ";
    char string2[] = "Welt";
    char string3[20];

    strcpy(string3, string1);
    strcat(string3, string2);
    cout << string3; // Ausgabe: Hallo Welt

    return 0;
}
```

In diesem Beispiel wird der ursprüngliche String "string1" zuerst in einen neuen leeren String "string3" kopiert und dann mit "string2" verbunden. Die Funktion "strcpy()" kopiert den Inhalt von "string1" in "string3" und "strcat()" fügt den Inhalt von "string2" an "string3" an.

## Siehe auch

- [C++ Strings](https://www.programiz.com/cpp-programming/string)
- [C++ Strings verbinden und trennen](https://www.geeksforgeeks.org/c-format-specifiers/)
- [String Library in C++](https://www.cplusplus.com/reference/string/)