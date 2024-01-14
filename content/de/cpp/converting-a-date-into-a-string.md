---
title:    "C++: Ein Datum in einen String umwandeln"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von einem Datum in einen String ist eine häufige Aufgabe in der Programmierung. Es wird oft benötigt, um Nutzern die Darstellung eines Datums auf eine für sie verständliche Art und Weise zu präsentieren oder um es in einem bestimmten Format für Datenbanken oder APIs zu speichern.

## Wie es funktioniert

Die Umwandlung eines Datums in einen String kann auf verschiedene Arten erfolgen, abhängig von der Programmiersprache und der Bibliothek, die verwendet wird. Hier werden wir uns auf die Verwendung von C++ und der Standardbibliothek "chrono" konzentrieren.

Um ein Datum in einem bestimmten Format zu speichern, müssen zunächst die Werte für Jahr, Monat und Tag definiert werden. Dies kann mit der Klasse "std::tm" erreicht werden, die in der "time.h" Bibliothek enthalten ist. Dann kann mit Hilfe der Klasse "std::chrono::system_clock" ein Zeitpunkt angegeben werden, aus dem das Datum extrahiert werden soll.

Im folgenden Beispiel umwandeln wir das aktuelle Datum in den String "DD.MM.YYYY":

```C++
#include <iostream>
#include <string>
#include <ctime>
#include <chrono>

int main() {
    // Definiere ein Objekt der Klasse struct tm, welches Jahr, Monat und Tag enthält
    std::tm date = { 0, 0, 0, 7, 5, 2020 - 1900 };
    
    // Erstelle ein Objekt der Klasse chrono::system_clock und speichere den aktuellen Zeitpunkt
    std::chrono::system_clock::time_point tp = std::chrono::system_clock::from_time_t(std::mktime(&date));
    
    // Verwende die strftime() Funktion, um das Datum in einen String zu konvertieren
    char strDate[11]; // Speichere den String mit 11 Zeichen (TT.MM.JJJJ\0)
    std::strftime(strDate, 11, "%d.%m.%Y", &date);
    
    // Gib den konvertierten String aus
    std::cout << strDate; // Output: 07.05.2020
    return 0;
}
```

## Tiefergehende Information

Wenn du tiefer in das Konzept der Umwandlung von Daten in Strings eintauchen möchtest, gibt es einige Dinge zu beachten. Zum Beispiel ist es wichtig, das richtige Datenformat zu wählen, um Missverständnisse zu vermeiden oder Probleme mit unterschiedlichen Sprachen und Ländern zu vermeiden. Außerdem gibt es verschiedene Funktionen und Methoden, die je nach Programmiersprache und Bibliothek verwendet werden können, um die Umwandlung zu erleichtern.

Ein weiterer wichtiger Aspekt bei der Umwandlung von Datum in String ist die Behandlung von Zeit- und Datumssprüngen, wie beispielsweise bei der Umstellung auf Sommer- oder Winterzeit. Es ist wichtig, sicherzustellen, dass die Umwandlung immer einheitlich und korrekt erfolgt, um unerwartete Ergebnisse zu vermeiden.

## Siehe auch

- [ISO-FORMAT - Die internationale Norm für Datumsangaben](https://www.iso.org/iso-8601-date-and-time-format.html)
- [strftime() Funktion in C++](https://en.cppreference.com/w/cpp/chrono/c/strftime)
- [Strukturiertes Datum in String umwandeln](https://www.codeproject.com/Tips/323722/Converting-Structured-Date-into-String-in-Cplusplus)