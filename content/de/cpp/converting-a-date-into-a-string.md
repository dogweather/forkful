---
title:                "Umwandlung eines Datums in eine Zeichenkette"
html_title:           "C++: Umwandlung eines Datums in eine Zeichenkette"
simple_title:         "Umwandlung eines Datums in eine Zeichenkette"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum 

Werfen wir einen Blick auf die Notwendigkeit, ein Datum in einen String umzuwandeln. Oftmals müssen wir Daten in einem bestimmten Format ausgeben, sei es für eine Benutzeroberfläche, eine Datei oder eine Datenbank. Durch die Umwandlung in einen String können wir sicherstellen, dass das Datum in der gewünschten Form angezeigt wird.

## Wie das geht

Die Konvertierung eines Datums in einen String in C++ kann mit der Standardbibliotheksfunktion `to_string()` erfolgen. Zunächst müssen wir jedoch das Datum als  `tm`-Struktur definieren und mit Werten füllen. Hier ist ein Beispielcode, der das aktuelle Datum in einen String im Format "TT/MM/JJJJ" umwandelt:

```C++
#include <iostream>
#include <sstream>
#include <iomanip>
#include <ctime>

int main() {
    // Definition einer tm-Struktur für das aktuelle Datum
    std::tm date;
    // Abrufen des aktuellen Datums und Zuweisen an die Struktur
    std::time_t t = std::time(0);
    date = *std::localtime(&t);

    // Konvertierung in einen String
    std::stringstream ss;
    ss << std::put_time(&date, "%d/%m/%Y");
    std::string date_string = ss.str();
    
    // Ausgabe des Strings
    std::cout << "Das Datum im Format TT/MM/JJJJ lautet: " << date_string << std::endl;
    return 0;
}
```

Dieser Code verwendet die Funktion `put_time()`, um das `tm`-Datum basierend auf dem angegebenen Format in einen String zu schreiben. In diesem Fall verwenden wir `%d` für den Tag, `%m` für den Monat und `%Y` für das Jahr.

Die Ausgabe des obigen Beispiels wäre:
```
Das Datum im Format TT/MM/JJJJ lautet: 13/11/2021
```

## Tiefergehende Informationen

Es ist wichtig zu beachten, dass die oben genannte Methode zur Zeit nur mit dem Compiler C++11 oder höher kompatibel ist. Vorherige Versionen von C++ unterstützen diese Funktion nicht.

Es gibt auch andere Möglichkeiten, ein Datum in einen String zu konvertieren, wie zum Beispiel die Verwendung von `strftime()` oder das Ausführen von benutzerdefiniertem Code, um die Konvertierung durchzuführen. Es ist wichtig, die richtige Methode je nach Anwendungsfall auszuwählen.

## Siehe auch

- [CppReference: to_string() Funktion](https://en.cppreference.com/w/cpp/string/basic_string/to_string)
- [CppReference: put_time() Funktion](https://en.cppreference.com/w/cpp/io/manip/put_time)
- [CppReference: strftime() Funktion](https://en.cppreference.com/w/cpp/chrono/c/strftime)