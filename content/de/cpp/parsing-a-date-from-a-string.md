---
title:                "Einen Datum aus einem String parsen"
html_title:           "Elixir: Einen Datum aus einem String parsen"
simple_title:         "Einen Datum aus einem String parsen"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was und Warum?
Parsing eines Datums aus einem String bedeutet, dass man eine Datumseinheit aus einer gegebenen Zeichenkette extrahieren und interpretieren kann. Programmierer nutzen dies, um Daten in einem konsumierbaren Format zu manipulieren oder zu verarbeiten.

## So geht's:

Sie können `std::istringstream` und `std::get_time` aus der Standardbibliothek verwenden, um ein Datum in C++ zu parsen. Hier ist ein einfacher Code, der dies verdeutlicht:

```C++
#include <iostream>
#include <sstream>
#include <iomanip>
#include <ctime>

int main() {
    std::string s = "21-06-2021"; //dd-mm-yyyy
    std::tm tm = {};
    std::istringstream ss(s);
    ss >> std::get_time(&tm, "%d-%m-%Y");
  
    if (ss.fail()) {
        std::cout << "Parse fehlgeschlagen!\n";
    } else {
        std::cout << std::put_time(&tm, "%c") << '\n';
    }
    return 0;
}
```

Wenn Sie dieses Programm ausführen, erhalten Sie die Ausgabe:

```
Mon Jun 21 00:00:00 2021
```

## Deep Dive:

Das Parsen von Daten aus Zeichenketten wurde historisch für alles verwendet, von Webentwicklung bis hin zu maschinellem Lernen. Es hat Entwicklern geholfen, komplexe Datumsangaben zu handhaben und sie flexibel und verwertbar zu machen.

In C++ können Sie auch Bibliotheken wie Boost verwenden, um ähnliche Funktionen zu ermöglichen. Boost bietet eine robustere Erfahrung und mehr Unterstützung für verschiedene Datumsformate.

Eine wichtige Einzelheit in der Implementierung ist, dass `std::get_time` keinerlei Ausnahme wirft, wenn das Parsen fehlschlägt. Stattdessen setzen wir die Failbit des `std::istringstream`-Objekts, welches durch die Methode `fail()` überprüft werden kann.

## Siehe Auch:

- [Cplusplus.com get_time](http://www.cplusplus.com/reference/iomanip/get_time/)
- [StackOverflow - How to parse date properly](https://stackoverflow.com/questions/5286945/timestamp-string-to-time-t-in-c)
- [Boost Date Time Library](https://www.boost.org/doc/libs/1_67_0/doc/html/date_time.html)