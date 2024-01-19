---
title:                "Berechnung eines zukünftigen oder vergangenen Datums"
html_title:           "C++: Berechnung eines zukünftigen oder vergangenen Datums"
simple_title:         "Berechnung eines zukünftigen oder vergangenen Datums"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?
Beim Berechnen eines zukünftigen oder vergangenen Datums manipulieren wir Daten, um eine bestimmte Anzahl von Tagen, Monaten oder Jahren hinzuzufügen oder abzuziehen. Programmierer tun dies oft, um Ablaufdaten, geplante Ereignisse oder Fristen zu berechnen.

## Wie zu:
Werfen wir einen Blick auf ein praktisches Codebeispiel:

```C++
#include <iostream>
#include <ctime>
#include <chrono>

int main() 
{
    std::time_t now = std::chrono::system_clock::to_time_t(std::chrono::system_clock::now());
    struct tm newdate = *std::localtime(&now);
    newdate.tm_mday += 30; // Berechne das Datum 30 Tage in der Zukunft
    std::time_t future = std::mktime(&newdate);

    // Ausgabe: Das aktuelle Datum und 30 Tage zukünftiges Datum
    char str_now[20];
    strftime(str_now, 20, "%d.%m.%Y", std::localtime(&now));
    std::cout << "Heutiges Datum: " << str_now << '\n';

    char str_future[20];
    strftime(str_future, 20, "%d.%m.%Y", std::localtime(&future));
    std::cout << "Zukünftiges Datum: " << str_future << '\n';

    return 0;
}
```
Beispiel-Ausgabe:

```C++
Heutiges Datum: 01.05.2022
Zukünftiges Datum: 31.05.2022
```

## Tief Tauchen
**Historischer Hintergrund**: Historisch gesehen wurde diese Methode entwickelt, um mit dem Bedarf von Geschäfts- und Unternehmensanwendungen umzugehen. Diese Anwendungen müssen oft vergangene, gegenwärtige und zukünftige Daten verwalten.

**Alternativen**: Es gibt mehrere Wege, um dieses Ziel zu erreichen. Andere Optionen umfassen die Verwendung von Bibliotheken wie Boost.Date_Time oder datetime in Python, die Berechnungen wie diese transparenter machen können.

**Implementierungsdetails**: Beachten Sie, dass in unserem Codebeispiel das mktime-Funktion rollt über, wenn wir über den aktuellen Monat hinaus Tage hinzufügen. Wenn wir beispielsweise am 30. April 30 Tage hinzufügen, gibt mktime uns den 30. Mai zurück.

## Siehe auch
**C++ Referenz - `mktime`:** https://en.cppreference.com/w/cpp/chrono/c/mktime

**C++ Referenz - `strftime`:** https://en.cppreference.com/w/cpp/chrono/c/strftime

**Boost.Date_Time-Dokumentation:** https://www.boost.org/doc/libs/1_77_0/doc/html/date_time.html

Erforschen Sie diese Ressourcen, um Ihre Kenntnisse zu vertiefen und mehr über die verschiedenen Optionen zur Berechnung von zukünftigen und vergangenen Daten in C++ zu erfahren.