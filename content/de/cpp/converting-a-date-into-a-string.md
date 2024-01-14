---
title:    "C++: Konvertieren eines Datums in einen String"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum
Die Konvertierung eines Datums in einen String ist eine häufige Aufgabe beim Programmieren, insbesondere beim Umgang mit Datums- oder Zeitfunktionen. Sie kann verwendet werden, um Daten in einem leserlichen Format darzustellen oder um weitere Berechnungen durchzuführen.

## Wie geht das
```C++
#include <iostream>
#include <string>
#include <ctime>

int main() {
    std::time_t now = std::time(NULL); 
    // Zeitstempel des aktuellen Datums
    char date[40]; 

    // Datumsformatierung
    std::strftime(date, sizeof(date), "%d.%m.%Y", std::localtime(&now)); 
    // "04.12.2021"

    // Ausgabe des Datums als String
    std::cout << "Das heutige Datum ist " << date << std::endl;

    return 0;
}
```

## Tiefer Einblick
Bei der Konvertierung eines Datums in einen String gibt es einige wichtige Schritte zu beachten. Zunächst muss das Datum als Zeitstempel gespeichert werden, damit es vom Computer verarbeitet werden kann. Dies kann mit der Funktion `time()` erreicht werden. Anschließend wird das Datum in das gewünschte Format gebracht, indem die Funktion `strftime()` verwendet wird. Hier können verschiedene Symbole und Zeichen verwendet werden, um das Datum im gewünschten Format darzustellen. Schließlich wird das formatierte Datum in einen String gespeichert und kann ausgegeben werden. Es ist wichtig zu beachten, dass die Verwendung von Datumsfunktionen von der verwendeten Plattform abhängig sein kann und dass die Formatierung von Datum und Uhrzeit differenzieren kann.

## Siehe auch
- [C++ Datums- und Zeitfunktionen](https://www.cplusplus.com/reference/ctime/)
- [Online-Datumskonverter](https://www.onlinedatumskonverter.de/) für das Umrechnen von Datum und Uhrzeit in verschiedene Formate
- [Tutorial: Einführung in die C++ Programmierung](https://www.tutorialspoint.com/cplusplus/index.htm) für weitere Grundlagen der C++ Programmierung.