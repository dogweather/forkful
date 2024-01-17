---
title:                "Das Abrufen des aktuellen Datums"
html_title:           "C++: Das Abrufen des aktuellen Datums"
simple_title:         "Das Abrufen des aktuellen Datums"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Was & Warum?

Das Abrufen des aktuellen Datums ist eine häufige Aufgabe für Programmierer. Es ermöglicht ihnen, das Datum zu einem bestimmten Zeitpunkt in ihrem Programm zu kennen und entsprechend darauf zu reagieren. Oft wird das aktuelle Datum verwendet, um Logfiles zu datieren, den Benutzer über wichtige Ereignisse zu informieren oder um eine zeitbasierte Funktionalität zu implementieren, wie z.B. das Anzeigen des aktuellen Datums auf einer Webseite.

# Wie geht das?

Um das aktuelle Datum in C++ abzurufen, gibt es mehrere Möglichkeiten. Eine davon ist die Verwendung der `ctime` Bibliothek. Hier ist ein Beispielcode, der das Datum in einem bestimmten Format ausgibt:

```C++
#include <iostream>
#include <ctime>

int main()
{
    // Struktur für das aktuelle Datum und die Zeit
    time_t now = time(0);

    // Konvertiert das aktuelle Datum und die Zeit in ein lesbare Form
    char* dt = ctime(&now);

    // Gibt das Datum und die Zeit aus
    std::cout << "Das aktuelle Datum und die Zeit ist: " << dt << std::endl;

    return 0;
}
```

Die Ausgabe dieses Codes könnte wie folgt aussehen: 

```bash
Das aktuelle Datum und die Zeit ist: Wed Jun 23 12:07:54 2021
```

Es ist wichtig zu beachten, dass das aktuelle Datum und die Zeit von dem Zeitpunkt abhängig sind, an dem der Code ausgeführt wird.

Eine andere Möglichkeit, das aktuelle Datum abzurufen, ist die Verwendung der `chrono` Bibliothek. Diese ermöglicht es, das Datum in einem bestimmten Format zu speichern und zu manipulieren. Hier ist ein Beispielcode, der das aktuelle Datum in einem benutzerdefinierten Format ausgibt:

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main()
{
    // Zeitpunkt für das aktuelle Datum
    std::chrono::system_clock::time_point now = std::chrono::system_clock::now();

    // Wandelt den Zeitpunkt in ein Datum und Uhrzeit-Objekt um
    std::time_t now_c = std::chrono::system_clock::to_time_t(now);

    // Benutzerdefiniertes Format für das Datum und die Uhrzeit
    std::cout << "Das aktuelle Datum und die Zeit ist: " << std::put_time(std::localtime(&now_c), "%Y-%m-%d %H:%M:%S") << std::endl;

    return 0;
}
```

Die Ausgabe des Codes würde wie folgt aussehen:

```bash
Das aktuelle Datum und die Zeit ist: 2021-06-23 12:07:54
```

# Tief tauchen

Das Abrufen des aktuellen Datums hat eine lange Geschichte in der Programmierung. Früher wurde es oft verwendet, um Dateien mit dem aktuellen Datum zu benennen, da die meisten Dateisysteme nur acht Zeichen für den Dateinamen zuließen. Heutzutage wird es hauptsächlich zur Verwendung in der Zeitzonenberechnung oder zur Implementierung von zeitabhängigen Funktionen verwendet.

Eine Alternative zur Verwendung der `ctime` und `chrono` Bibliotheken ist die Verwendung von externen Bibliotheken, wie z.B. Boost.Date_Time oder ICU (International Components for Unicode), die umfangreiche Funktionen zur Manipulation von Datum und Uhrzeit bieten.

Die Implementierung des Abrufs des aktuellen Datums kann je nach Betriebssystem variieren. Die meisten Betriebssysteme bieten jedoch Systemfunktionen oder APIs zur Verfügung, um das aktuelle Datum abzurufen.

# Siehe auch

Hier sind einige hilfreiche Links zum Thema "Aktuelles Datum abrufen":

- [cplusplus.com - ctime library](http://www.cplusplus.com/reference/ctime/)
- [cplusplus.com - chrono library](http://www.cplusplus.com/reference/chrono/)
- [Boost.Date_Time](https://www.boost.org/doc/libs/1_76_0/doc/html/date_time.html)
- [ICU - Date and Time](https://unicode-org.github.io/icu/userguide/datetime/)