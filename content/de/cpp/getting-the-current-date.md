---
title:                "Das aktuelle Datum abrufen"
html_title:           "Gleam: Das aktuelle Datum abrufen"
simple_title:         "Das aktuelle Datum abrufen"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Abrufen des aktuellen Datums ist ein weit verbreitetes Verfahren, bei dem der aktuelle Tag, Monat und Jahr vom Computer in Echtzeit abgerufen wird. Dies ist für viele Anwendungsfälle nützlich, z.B. zur Zeiterfassung, zur Generierung von Zeitstempeln oder zur Durchführung von Berechnungen, die das aktuelle Datum erfordern.

## So geht's:

In C++ können Sie das aktuelle Datum über die `<chrono>`-Bibliothek abrufen. Hier ist ein kurzes Beispiel:

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    auto currentTime = std::chrono::system_clock::now();
    std::time_t date = std::chrono::system_clock::to_time_t(currentTime);

    std::cout << "Das aktuelle Datum ist: " << std::ctime(&date);
    return 0;
}
```

Wenn Sie das ausführen, könnte die Ausgabe wie folgt aussehen:

```
Das aktuelle Datum ist: Tue Jun 15 14:33:59 2021
```

## Tiefere Einblicke

#### Historischer Kontext
In älteren C++-Versionen wurde das aktuelle Datum meist über die `<ctime>`-Bibliothek mit Funktionen wie `time()` und `localtime()` abgerufen. Mit C++11 kamen die `<chrono>`-Bibliothek und verbesserte Verfahren zum Abrufen des aktuellen Datums hinzu.

#### Alternativen
Alternativ können Sie nach wie vor die Funktionen `time()` und `localtime()` der `<ctime>`-Bibliothek verwenden, um das aktuelle Datum zu ermitteln.

```C++
#include <ctime>
#include <iostream>

int main() {
    std::time_t now = std::time(nullptr);
    std::tm *ltm = localtime(&now);

    std::cout << "Heute ist: " << ltm->tm_mday <<"."<< 1 + ltm->tm_mon <<"."<< 1900 + ltm->tm_year;
    return 0;
}
```

#### Implementierungsdetails
Die `system_clock`-Funktion in der `<chrono>`-Bibliothek gibt die Zeit seit dem Epoch (00:00:00 UTC, 1. Januar 1970) zurück. Mit der Funktion `to_time_t` wird diese Zeit in Sekunden seit dem Epoch ausgegeben, was vom `ctime` zur Generierung eines lesbaren Datumsstrings genutzt wird.

## Siehe Auch

- C++ `<chrono>`-Bibliothek: [cplusplus.com](https://www.cplusplus.com/reference/chrono/)
- C++ `<ctime>`-Bibliothek: [cplusplus.com](https://www.cplusplus.com/reference/ctime/)
- Mehr über das Unix Epoch: [Wikipedia](https://de.wikipedia.org/wiki/Unixzeit)