---
title:                "Umwandeln eines Strings in Kleinbuchstaben"
html_title:           "C++: Umwandeln eines Strings in Kleinbuchstaben"
simple_title:         "Umwandeln eines Strings in Kleinbuchstaben"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Was & Warum?
Das Konvertieren eines Strings in Kleinbuchstaben ist ein häufiger Schritt in der Programmierung, bei dem ein String in eine einheitliche Schreibweise gebracht wird. Programmierer nutzen dies, um Vergleiche oder Suchanfragen zu erleichtern, da Groß- und Kleinschreibung oft keine Rolle spielt und verschiedene Einheiten so zu einer Einheit werden.

# Wie geht's?
```C++ 
// Beispielcode für die Konvertierung eines Strings in Kleinbuchstaben
#include <iostream>
#include <algorithm>
#include <string>
using namespace std;

int main() {
    // Eingangsstring definieren
    string s = "HALLO WELT";
    
    // String in Kleinbuchstaben umwandeln mit transform 
    transform(s.begin(), s.end(), s.begin(), ::tolower);
    
    // Ausgabe des konvertierten Strings
    cout << s;
    
    return 0;
}

// Output: hallo welt
```

# Tiefes Eintauchen
Die Verwendung von Groß- und Kleinschreibung in der Sprachverarbeitung ist eine relativ neue Entwicklung. Frühere Programmiersprachen hatten dies nicht und daher war die Konvertierung von Strings in der Praxis nicht relevant. In C++ gibt es verschiedene Möglichkeiten, um einen String in Kleinbuchstaben umzuwandeln, einschließlich der Verwendung von Bibliotheksfunktionen wie transform oder der Verwendung von Schleifen. Es gibt auch Alternativen wie die Verwendung von regulären Ausdrücken oder die Verwendung von Standardfunktionen wie tolower.

# Weitere Quellen
* Mehr über die Verarbeitung von Strings in C++: [C++ Strings](https://www.programiz.com/cpp-programming/string)
* Eine detaillierte Erklärung zum Konzept von Groß- und Kleinschreibung: [Wikipedia - Case Sensitivity](https://de.wikipedia.org/wiki/Gro%C3%9F-_und_Kleinschreibung)