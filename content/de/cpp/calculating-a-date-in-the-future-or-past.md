---
title:                "Eine Datum in der Zukunft oder Vergangenheit berechnen"
html_title:           "C++: Eine Datum in der Zukunft oder Vergangenheit berechnen"
simple_title:         "Eine Datum in der Zukunft oder Vergangenheit berechnen"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum?

Möglichkeiten zur Berechnung von zukünftigen oder vergangenen Daten können hilfreich sein, um Zeitangaben in Programmen oder Skripten zu automatisieren oder um genaue Zeitberechnungen durchzuführen.

## Wie geht es?

Das Berechnen von zukünftigen oder vergangenen Daten in C++ ist relativ einfach und erfordert nur wenige Zeilen Code. Hier ist ein Beispiel, das zeigt, wie man drei Monate zu einem bestimmten Datum hinzufügt:

```C++
#include <iostream>
#include <chrono> // Bibliothek für Datum und Zeit

int main()
{
  // Datum festlegen
  std::chrono::system_clock::time_point date{ std::chrono::system_clock::now() }; 

  // Drei Monate hinzufügen
  date += std::chrono::months{ 3 };

  // Ausgabe des Ergebnisses
  std::cout << "In drei Monaten ist es der " << date << "\n";
  return 0;
}
```
Die Ausgabe sieht folgendermaßen aus:
```
In drei Monaten ist es der 2021-03-02 14:49:28.857661
```

## Tiefergehen

Um genaue Berechnungen zu zukünftigen oder vergangenen Daten durchzuführen, ist es wichtig zu verstehen, wie Zeit in C++ dargestellt wird. In der Regel wird die Zeit als Anzahl von Sekunden oder Nanosekunden seit dem 1. Januar 1970 gespeichert. Mit Hilfe von Bibliotheken wie "chrono" können wir diese Zeitangaben in verschiedene Formate umwandeln, um unsere Berechnungen durchzuführen. Es ist auch wichtig zu beachten, dass Zeitzonen und Sommerzeit berücksichtigt werden müssen, um genaue Ergebnisse zu erzielen.

## Siehe auch

- [Guide zu Datum und Zeit in C++](https://www.learncpp.com/cpp-tutorial/8-11-a-simple-date-class/)
- [Chrono Dokumentation](https://en.cppreference.com/w/cpp/chrono)
- [Zeitrechnung in C++ verstehen](https://www.educative.io/edpresso/how-to-use-dates-and-time-properly-in-cpp)