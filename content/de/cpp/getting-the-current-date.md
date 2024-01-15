---
title:                "Das aktuelle Datum erhalten"
html_title:           "C++: Das aktuelle Datum erhalten"
simple_title:         "Das aktuelle Datum erhalten"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum
Wer C++ programmiert, wird häufig auf die Notwendigkeit stoßen, das aktuelle Datum in sein Programm einzufügen. Dies kann aus verschiedenen Gründen erforderlich sein, zum Beispiel zur Datumsberechnung oder zur Protokollierung von Aktionen.

## Wie man das aktuelle Datum abruft
Es gibt verschiedene Möglichkeiten, das aktuelle Datum in C++ abzurufen. Hier werden zwei gängige Methoden mit Beispielen vorgestellt.

### Methode 1: Die `time()` Funktion
Die `time()` Funktion ist eine Standard-Header-Funktion, die die Anzahl der Sekunden seit dem 01.01.1970 zurückgibt. Um das aktuelle Datum zu erhalten, muss diese Sekundenanzahl in ein `time_t` Objekt umgewandelt werden und anschließend in eine `struct tm` Struktur zerlegt werden. Im folgenden Beispiel wird das aktuelle Datum in dem Format Jahr-Monat-Tag ausgegeben:

```C++
#include <iostream>
#include <ctime>

int main()
{
    // Deklaration von Variablen
    time_t now;         // Sekunden seit 01.01.1970
    time(&now);         // Aktuelle Anzahl an Sekunden wird in "now" gespeichert
    tm *date = localtime(&now);  // Umwandlung in eine "struct tm" Struktur
    
    // Ausgabe des aktuellen Datums in dem Format Jahr-Monat-Tag
    std::cout << "Das aktuelle Datum lautet: " << (date->tm_year + 1900) << "-" << (date->tm_mon + 1) << "-" << date->tm_mday << std::endl;
    
    return 0;
}
```

#### Ausgabe:
Das aktuelle Datum lautet: 2021-03-15

### Methode 2: Die `chrono` Bibliothek
Die `chrono` Bibliothek bietet eine etwas modernere Möglichkeit, auf das aktuelle Datum zuzugreifen. Dabei wird die `system_clock` verwendet, um die aktuelle Zeit zu ermitteln. Im folgenden Beispiel wird das Datum in dem Format Tag-Monat-Jahr, sowie die aktuelle Uhrzeit in dem Format Stunden:Minuten ausgegeben:

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main()
{
    // Deklaration von Variablen
    auto now = std::chrono::system_clock::now();  // Aktuelle Zeit
    std::time_t time = std::chrono::system_clock::to_time_t(now);
    std::tm *date = std::localtime(&time);    // Umwandlung in eine "tm" Struktur
    
    // Ausgabe des aktuellen Datums in dem Format Tag-Monat-Jahr
    std::cout << "Das aktuelle Datum lautet: " << date->tm_mday << "-" << (date->tm_mon + 1) << "-" << (date->tm_year + 1900) << std::endl;
    
    // Ausgabe der aktuellen Uhrzeit in dem Format Stunden:Minuten
    std::cout << "Die aktuelle Uhrzeit lautet: " << date->tm_hour << ":" << date->tm_min << std::endl;
    
    return 0;
}
```

#### Ausgabe:
Das aktuelle Datum lautet: 15-03-2021
Die aktuelle Uhrzeit lautet: 12:00

## Tiefergehende Informationen
Beide Methoden liefern das aktuelle Datum in dem Format Datum-Monat-Jahr. Dabei ist zu beachten, dass die `time()` Funktion das Jahr als eine Anzahl von Jahren seit 1900 zurückgibt, während die `chrono` Bibliothek das Jahr als das tatsächliche Jahr ausgibt. Zusätzlich gibt die `localtime()` Funktion das Datum in einer `tm` Struktur zurück, welche verschiedene Elemente wie Jahr, Monat, Tag, Stunde, Minute, Sekunde, Wochentag usw. enthält.

## Siehe auch
- [C++ time() Dokumentation](https://www.cplusplus.com/reference/ctime/time/)
- [C++ chrono Bibliothek Dokumentation](https://www.cplusplus.com/reference/chrono/)