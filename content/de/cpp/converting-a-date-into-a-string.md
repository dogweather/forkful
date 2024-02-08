---
title:                "Datum in einen String umwandeln"
aliases:
- de/cpp/converting-a-date-into-a-string.md
date:                  2024-01-20T17:36:13.718754-07:00
model:                 gpt-4-1106-preview
simple_title:         "Datum in einen String umwandeln"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Datum zu String-Konversion ist der Prozess der Umwandlung eines Datumsobjekts in eine lesbare Zeichenkette. Programmierer machen das, um Daten benutzerfreundlich anzuzeigen oder für den Export und die Weiterverarbeitung zu formatieren.

## So geht’s:
```C++
#include <iostream>
#include <iomanip>
#include <sstream>
#include <ctime>

int main() {
    // Aktuelles Datum und Uhrzeit erhalten
    std::time_t t = std::time(nullptr);
    std::tm *tm_ptr = std::localtime(&t);

    // Mit stringstream in String konvertieren
    std::stringstream ss;
    ss << std::put_time(tm_ptr, "%d.%m.%Y");

    // String ausgeben
    std::string date_as_string = ss.str();
    std::cout << "Heutiges Datum: " << date_as_string << std::endl;

    // Alternative mit C++20
    // std::format wird in zukünftigen Versionen verfügbar
    // std::string date_formatted = std::format("{:%d.%m.%Y}", *tm_ptr);
    // std::cout << "Heutiges Datum: " << date_formatted << std::endl;

    return 0;
}
```
**Ausgabebeispiel:**
```
Heutiges Datum: 01.04.2023
```

## Deep Dive
Die Konversion von Datum zu String ist nicht neu und hat sich mit der Zeit entwickelt. Ursprünglich nutzte man `strftime()`, eine Funktion aus der C Standardbibliothek. Mit C++ wurde das `std::put_time` und der `stringstream` eingeführt, die eine Objekt-orientiertere Herangehensweise bieten. In C++20 kam `std::format`, eine sicherere und flexiblere Alternative, die momentan aber noch nicht breit unterstützt ist.

Man muss auch Zeitzonen beachten. `std::localtime` berücksichtigt die lokale Zeitzone, während `std::gmtime` die GMT/UTC Zeit liefert.

Alternativen sind Bibliotheken wie `boost::date_time` oder `fmt` für ältere C++ Versionen sowie `std::chrono` aus C++11 und später für hochpräzise Zeitmessungen.

## Siehe Auch
- [cppreference.com, std::put_time](https://en.cppreference.com/w/cpp/io/manip/put_time)
- [cppreference.com, std::format (C++20)](https://en.cppreference.com/w/cpp/utility/format/format)
- [Boost.Date_Time Dokumentation](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html)
- [fmtlib Dokumentation](https://fmt.dev/latest/index.html)
