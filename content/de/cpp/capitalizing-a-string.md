---
title:                "C++: Eine Zeichenkette großschreiben"
simple_title:         "Eine Zeichenkette großschreiben"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Warum: 

Das Kapitalisieren von Zeichenketten ist eine grundlegende Aufgabe in der C++ Programmierung und kann dabei helfen, den Code übersichtlicher zu gestalten. Durch die Verwendung von Großbuchstaben anstelle von Kleinbuchstaben kann auch die Lesbarkeit verbessert werden.

Wie man es macht:

```C++
#include <iostream> 
#include <string> 

using namespace std; 

int main() 
{ 
    string name = "hallo welt"; 
    string capitalized_name = ""; 

    for (int i = 0; i < name.length(); i++) { 
        capitalized_name += toupper(name[i]); 
    } 

    cout << capitalized_name << endl; // Ausgabe: HALLO WELT

    return 0; 
} 
```

Tiefergehende Informationen:

Das Kapitalisieren von Zeichenketten erfolgt durch die Verwendung von Schleifen und der `toupper()` Funktion aus der Standardbibliothek `string`. Diese Funktion wandelt jeden Buchstaben in einen Großbuchstaben um und fügt ihn zur neuen Zeichenkette hinzu. Hierbei ist es wichtig zu beachten, dass die Variablen `name` und `capitalized_name` vom gleichen Datentyp `string` sein müssen.

Weitere Möglichkeiten zur Kapitalisierung von Zeichenketten sind die Verwendung von Funktionen wie `transform()` oder die Nutzung von booleschen Ausdrücken in einer Schleife, um spezielle Zeichen zu beachten und entsprechend umzuwandeln.

Siehe auch:

- [C++ Strings](https://www.cplusplus.com/reference/string/string/)
- [toupper() Funktion](https://www.cplusplus.com/reference/cctype/toupper/)