---
title:                "Verknüpfung von Zeichenfolgen"
html_title:           "C++: Verknüpfung von Zeichenfolgen"
simple_title:         "Verknüpfung von Zeichenfolgen"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?

Concatenating Strings bedeutet, zwei oder mehr Strings aneinander anzuhängen, um einen neuen, längeren String zu erstellen. Programmierer machen dies oft, um längere Texte zu erstellen oder um Texte mit Variablen oder Nutzereingaben zu kombinieren.

## Wie:

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
  // Beispiel 1:
  string str1 = "Hallo";
  string str2 = "Welt";
  string str3 = str1 + str2;
  cout << str3 << endl;
  // Ausgabe: HalloWelt
  
  // Beispiel 2:
  string name;
  cout << "Wie ist dein Name? ";
  cin >> name;
  string message = "Hallo " + name + ", schön dich zu treffen!";
  cout << message << endl;
  // Eingabe: Max
  // Ausgabe: Hallo Max, schön dich zu treffen!
  
  return 0;
}
```

## Tiefergehender Einblick:

Die Verkettung von Strings ist seit den Anfangstagen der Programmierung ein wichtiges Konzept. In den frühen Programmiersprachen wie FORTRAN und COBOL war die Verwendung von automatisch dimensionierten Zeichenfeldern die einzige Möglichkeit, Strings zu manipulieren. Heutzutage gibt es Alternativen zur Verkettung von Strings, wie z.B. die Verwendung von Stringstreams oder das Erstellen von Vektoren von Zeichenfolgen.

## Siehe auch:

- [String-Operationen in C++](https://www.cplusplus.com/reference/string/string/)
- [Stringstreams](https://www.cplusplus.com/reference/sstream/stringstream/)
- [Vektoren in C++](https://www.cplusplus.com/reference/vector/vector/)