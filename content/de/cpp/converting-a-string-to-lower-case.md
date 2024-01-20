---
title:                "Einen String in Kleinbuchstaben umwandeln"
html_title:           "Elm: Einen String in Kleinbuchstaben umwandeln"
simple_title:         "Einen String in Kleinbuchstaben umwandeln"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Umwandeln eines Strings in Kleinbuchstaben ist eine Operation, die den gesamten String in Kleinbuchstaben ändert. Programmierer tun dies oft, um die Eingabedaten zu vereinheitlichen und so den Vergleich von Textdaten zu erleichtern.

## So geht's:
In C++ können wir die `<algorithm>` Bibliothek und die `tolower` Funktion verwenden, um einen String problemlos in Kleinbuchstaben zu ändern. Hier ist ein einfaches Beispiel:

```C++
#include <algorithm>
#include <cctype>
#include <iostream>
#include <string>

int main() {
    std::string str = "Hallo Welt!";
    std::transform(str.begin(), str.end(), str.begin(), 
        [](unsigned char c){ return std::tolower(c); });

    std::cout << str << std::endl;

    return 0;
}
```

Die Ausgabe dieses Programms ist dann `hallo welt!`.

## Vertiefende Betrachtung
Die `tolower` Funktion existiert schon seit den Anfängen der C-Programmierung und wurde in die C++-Standardbibliothek übernommen. Alternativ könnten Sie auch eine manuelle Schleife implementieren, um durch den String zu gehen und jeden Buchstaben individuell zu verändern. Jedoch ist auch diese Methode in der `transform` Funktion präzis implementiert. Wichtig zu beachten ist, dass die `tolower` Funktion nur ASCII-Buchstaben korrekt behandelt. Für UTF-8 Zeichen sind zusätzliche Schritte oder Bibliotheken erforderlich.

## Siehe auch
1. [C++ transform() Funktion](https://en.cppreference.com/w/cpp/algorithm/transform)
2. [C++ tolower() Funktion](https://en.cppreference.com/w/cpp/string/byte/tolower)
3. [Zusätzliche Informationen zu UTF-8](https://de.wikipedia.org/wiki/UTF-8) 
4. [ASCII](https://de.wikipedia.org/wiki/American_Standard_Code_for_Information_Interchange)