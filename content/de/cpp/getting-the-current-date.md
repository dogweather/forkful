---
title:                "C++: Das aktuelle Datum erhalten"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

In der heutigen Welt, in der vieles digitalisiert ist, ist es oft wichtig, das aktuelle Datum zu kennen. Ob es darum geht, Verträge zu unterzeichnen, Erinnerungen zu setzen oder Daten in einer Datenbank zu speichern, das aktuelle Datum ist unerlässlich. In diesem Blog-Beitrag werden wir uns ansehen, wie man in C++ das aktuelle Datum abrufen kann.

## Wie es geht

Das Abrufen des aktuellen Datums in C++ ist relativ einfach. Zunächst müssen wir die "chrono" Bibliothek in unserem Code einbinden. Diese Bibliothek enthält die Funktionen, die wir benötigen, um das Datum zu bekommen. Dann können wir die Funktion `chrono::system_clock::now()` verwenden, um die aktuelle Systemzeit abzurufen. Schauen wir uns ein Beispiel an:

```C++
#include <iostream>
#include <chrono>

int main() {
    auto current_time = std::chrono::system_clock::now();
    std::time_t current_time_t = std::chrono::system_clock::to_time_t(current_time);

    // Ausgabe des aktuellen Datums
    std::cout << "Das aktuelle Datum ist: " << std::ctime(&current_time_t);

    return 0;
}
```

Diese wenigen Zeilen Code geben uns das aktuelle Datum auf unserem System aus. Es ist wichtig zu beachten, dass die Funktion `std::ctime()` das Datum in der lokalen Zeitzone des Systems umwandelt. Wenn Sie das Datum in einer bestimmten Zeitzone benötigen, müssen Sie eine andere Funktion verwenden.

## Tiefer Einblick

Die Funktion `chrono::system_clock::now()` gibt ein Objekt des Typs "chrono::time_point" zurück. Dieses Objekt enthält die Anzahl der vergangenen Sekunden seit dem 1. Januar 1970, auch bekannt als "Unix-Zeit". Mit der Funktion `system_clock::to_time_t()` können wir dieses Objekt in ein Objekt des Typs `std::time_t` umwandeln, das ein Datum und eine Uhrzeit darstellt. Wir können dann die Funktion `std::ctime()` verwenden, um dieses Objekt in eine lesbare Zeichenfolge zu konvertieren.

Es gibt auch andere Funktionen, die das aktuelle Datum zurückgeben, wie zum Beispiel `chrono::steady_clock::now()`, die eine konstante Zeit zurückgeben, unabhängig davon, ob das System angepasst wird oder nicht.

## Siehe auch

- [C++ Dokumentation: Funktionen zur Zeitmessung](https://de.cppreference.com/w/cpp/chrono)
- [Unix Time: Warum ist der 01.01.1970 wichtig?](https://www.howtogeek.com/509592/epoch-time-explained-what-is-it-and-why-is-it-used/)