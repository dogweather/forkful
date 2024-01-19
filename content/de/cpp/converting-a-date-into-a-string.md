---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Java: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Umwandlung eines Datums in einen String besteht darin, ein Datenformat in einen lesbaren Text umzuwandeln. Programmierer tun dies, um Daten in Benutzeroberflächen oder zur Speicherung und Übertragung zu präsentieren.

## So geht's:

Einfache Umwandlung eines Datums in einen String in C++ mit der Bibliothek `<chrono>` und `<iomanip>`:

```C++
#include <chrono>
#include <iostream>
#include <iomanip>

int main(){
    auto jetzt = std::chrono::system_clock::now();
    std::time_t zeitpunkt = std::chrono::system_clock::to_time_t(jetzt);
    std::cout << std::put_time(std::localtime(&zeitpunkt), "%Y-%m-%d %X") << '\n';
    return 0;
}
```

Der obige Code wird das aktuelle Datum und die Zeit im Format "YYYY-MM-DD hh:mm:ss" ausgeben.

## Tiefgehende Analyse:

Die Umwandlung von Daten in Zeichenfolgen in C++ ist ein Prozess, der durch die Anforderungen des digitalen Zeitalters eingeführt wurde, da Text als das menschenlesbare Medium gilt. Es gibt Alternativen zur `<chrono>` und `<iomanip>` Bibliothek zum Beispiel, die `<ctime>` Bibliothek. Mit der Bibliothek, kann man das mit der Funktion `asctime()` tun. Betreffend der Implementierungsdetails ist zu beachten, dass die Funktion `std::put_time()` nur die Lokalzeit und nicht die UTC-Zeit zurück gibt. 

## Siehe auch:

Es gibt verschiedene Online-Ressourcen, die weiteres Wissen zu diesem Thema vermitteln:

- [cppreference.com: Chrono-Library](https://en.cppreference.com/w/cpp/chrono)
- [cppreference.com: Iomanip-Library](https://en.cppreference.com/w/cpp/io/manip)
- [stackoverflow.com: How to convert time to string in C++?](https://stackoverflow.com/questions/16357999/current-date-and-time-as-string-using-c)