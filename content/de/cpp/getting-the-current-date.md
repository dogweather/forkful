---
title:                "C++: Die aktuelle Datumsangabe erhalten"
simple_title:         "Die aktuelle Datumsangabe erhalten"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Die Verwendung des aktuellen Datums ist ein wesentlicher Bestandteil der Programmierung. Es ermöglicht uns, zeitbezogene Funktionen in unsere Programme zu integrieren, wie z.B. die Anzeige des aktuellen Datums für Benutzer oder die Durchführung von Berechnungen basierend auf dem aktuellen Datum.

## Wie Man

Um das aktuelle Datum in C++ zu erhalten, müssen wir zuerst die Bibliothek <ctime> einbinden. Diese enthält Funktionen, die es uns ermöglichen, auf das Datum und die Uhrzeit zuzugreifen. Dann müssen wir eine Variable vom Typ "time_t" deklarieren, um das aktuelle Datum zu speichern. Wir können dies mit der Funktion "time()" tun, die das aktuelle Datum in Sekunden seit dem 1. Januar 1970 zurückgibt. Wir können auch die Funktionen "localtime()" oder "gmtime()" verwenden, um dieses Ergebnis in ein lesbares Format umzuwandeln.

```C++
#include <iostream>
#include <ctime>

int main()
{
    // Einbinden der <ctime> Bibliothek
    std::time_t t = std::time(0);

    // Konvertieren in ein lesbares Format
    std::tm* now = std::localtime(&t);

    // Ausgabe des aktuellen Datums
    std::cout << "Das aktuelle Datum ist: "
              << (now->tm_year + 1900) << '-'
              << (now->tm_mon + 1) << '-'
              << now->tm_mday
              << std::endl;

    return 0;
}

```

Die Ausgabe des obigen Codes könnte wie folgt aussehen:

```
Das aktuelle Datum ist: 2021-07-20
```

## Blick ins Detail

Wie bereits erwähnt, gibt die Funktion "time()" das aktuelle Datum in Sekunden seit dem 1. Januar 1970 zurück. Dies wird auch als "Epoch"-Zeitstempel bezeichnet. Die Funktion "localtime()" wandelt dieses Ergebnis in einen strukturierten Zeiger um, der Informationen wie Jahr, Monat und Tag enthält. Die Funktion "gmtime()" funktioniert auf die gleiche Weise, gibt jedoch die UTC-Zeit (koordinierte Weltzeit) zurück.

Die <ctime> Bibliothek enthält auch viele andere Funktionen und Konstanten, die mit Datum und Uhrzeit zusammenhängen. Sie können in der offiziellen C++ Dokumentation weitere Informationen darüber finden.

## Siehe Auch

- [C++ Zeit- und Datumsfunktionen](https://www.ibm.com/support/knowledgecenter/ssw_ibm_i_73/rtref/timew.htm)
- [C++ Zeit-Klasse](http://www.cplusplus.com/reference/chrono/system_clock/now/)
- [Offizielle C++ Dokumentation für <ctime>](https://en.cppreference.com/w/cpp/chrono/c)