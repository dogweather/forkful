---
title:                "Ein Datum aus einem String extrahieren"
html_title:           "C++: Ein Datum aus einem String extrahieren"
simple_title:         "Ein Datum aus einem String extrahieren"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Was & Warum?
Das Parsen eines Datums aus einem String ist eine gängige Aufgabe für Programmierer. Dabei geht es darum, ein Datum (z.B. 01.01.2021) aus einem String (z.B. "01-01-2021") zu extrahieren und es in ein für den Computer lesbares Format zu konvertieren. Dies ist notwendig, um mit Datumswerten in Programmen zu arbeiten, z.B. für die Berechnung von Altersangaben oder das Sortieren von Datumsangaben.

# Wie geht's?
Das Parsen eines Datums aus einem String kann auf verschiedene Weise erfolgen, abhängig von der verwendeten Programmiersprache. Im Folgenden wird ein Beispiel in C++ gezeigt:

```C++
#include <iostream>

using namespace std;

int main() {
    // Definieren des Datums-Strings
    string dateStr = "01-01-2021";

    // Extrahieren des Tages, Monats und Jahres als separate Strings
    string day = dateStr.substr(0,2);
    string month = dateStr.substr(3,2);
    string year = dateStr.substr(6,4);

    // Konvertieren der Strings in Integer-Werte
    int dayInt = stoi(day);
    int monthInt = stoi(month);
    int yearInt = stoi(year);

    // Ausgabe des geparsten Datums
    cout << "Geparstes Datum: " << dayInt << "." << monthInt << "." << yearInt << endl;

    return 0;
}
```

**Ausgabe:**
Geparstes Datum: 01.01.2021

In diesem Beispiel wird der Datei-Header ```<iostream>``` verwendet, um die Ein- und Ausgabe-Funktionen von C++ zu nutzen. Die Strings werden mit ```string``` definiert und die Funktion ```substr()``` wird verwendet, um bestimmte Teile des Strings zu extrahieren. Mit der Funktion ```stoi()``` werden die extrahierten Strings in Integer-Werte konvertiert.

# Tiefere Einblicke
Das Parsen von Datumswerten aus Strings ist eine gängige Aufgabe, die sich im Laufe der Zeit weiterentwickelt hat. Früher wurde dies vor allem manuell durchgeführt, indem der String angepasst und Teile davon extrahiert wurden. Heutzutage gibt es jedoch viele Bibliotheken und Funktionen in verschiedenen Programmiersprachen, die das Parsen von Datumswerten erleichtern.

Eine Alternative zum manuellen Parsen ist die Verwendung von regulären Ausdrücken (engl. regular expressions), die es ermöglichen, Muster in einem String zu finden und zu extrahieren. Diese eignen sich besonders gut für komplexe Datumsmuster.

Die Implementierung des Parsers hängt immer von der verwendeten Programmiersprache ab. In C++ ist es beispielsweise möglich, den String in ein ```std::tm```-Objekt zu konvertieren, das die Datumsinformationen in einer strukturierten Form enthält.

# Siehe auch
- https://www.cplusplus.com/reference/ctime/tm/ (Referenz zur Verwendung von ```std::tm``` in C++)
- https://www.cplusplus.com/reference/string/string/substr/ (Referenz zur Verwendung von ```substr()``` in C++)
- https://www.cplusplus.com/reference/string/regex/ (Referenz zur Verwendung von regulären Ausdrücken (engl. regular expressions) in C++)