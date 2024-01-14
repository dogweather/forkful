---
title:                "C++: Vergleich von zwei Daten."
simple_title:         "Vergleich von zwei Daten."
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Die Vergleichung von zwei Daten ist ein wichtiges Konzept in der Programmierung, da es oft erforderlich ist, zu überprüfen, ob ein bestimmtes Datum vor oder nach einem anderen Datum liegt. Das kann beispielsweise bei der Erstellung von Kalendern oder bei der Berechnung von Alter verwendet werden.

## Wie man zwei Daten vergleicht

Um zwei Daten in C++ zu vergleichen, gibt es verschiedene Möglichkeiten. Eine Möglichkeit ist die Verwendung der "std::chrono" Bibliothek, die Funktionen zur Manipulation von Zeit und Datumsangaben bietet.

Ein Beispielcode für die Verwendung von "std::chrono" ist wie folgt:

```C++
#include <iostream>
#include <chrono>

int main()
{
    // Erstellen Sie zwei Daten zum Vergleich
    auto date1 = std::chrono::system_clock::now();
    auto date2 = std::chrono::system_clock::now() - std::chrono::hours(24);

    // Vergleichen Sie die Daten
    if(date1 > date2)
    {
        std::cout << "Das erste Datum liegt nach dem zweiten Datum." << std::endl;
    }
    else
    {
        std::cout << "Das erste Datum liegt vor dem zweiten Datum." << std::endl;
    }

    return 0;
}
```

Die Ausgabe dieses Codes wird je nach den erstellten Daten variieren, aber es wird immer angezeigt, ob das erste Datum vor oder nach dem zweiten Datum liegt.

## Tiefere Einblicke

Bei der Vergleichung von Daten in C++ ist es wichtig zu beachten, dass es verschiedene Arten von Datumsangaben gibt, wie zum Beispiel lokale Zeit, UTC und UNIX-Zeit. Daher ist es wichtig, die richtigen Funktionen aus der "std::chrono" Bibliothek für die jeweilige Art von Datumsangabe zu verwenden.

Außerdem ist es wichtig zu beachten, dass bei der Verwendung von "std::chrono" die Genauigkeit der vergleichen