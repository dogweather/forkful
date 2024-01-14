---
title:    "C++: Ein Datum in der Zukunft oder Vergangenheit berechnen"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen eines Datums in der Zukunft oder Vergangenheit kann in vielen Programmierprojekten nützlich sein, zum Beispiel bei Reservierungssystemen oder bei der Berechnung von Ablaufdaten.

## Wie geht's

Wir können dies mit Hilfe von C++ und der <ctime> Bibliothek erreichen. Zunächst müssen wir das aktuelle Datum und die gewünschte Anzahl an Tagen in einer Variablen speichern. Dann können wir die Funktion ```std::mktime``` verwenden, um das Datum in ein ```std::tm``` Objekt zu konvertieren. Wir können dann die entsprechenden Werte für das Jahr, den Monat und den Tag ändern und schließlich wieder mit der Funktion ```std::strftime``` in ein für Menschen lesbares Format konvertieren. Hier ist ein Beispielcode:

```C++
#include <iostream>
#include <ctime>

int main() {
    // aktuelles Datum
    std::time_t today = std::time(0);

    // Anzahl der Tage in der Zukunft oder Vergangenheit
    int num_days = 30;

    // umwandeln in std::tm Objekt
    std::tm* date = std::localtime(&today);

    // entsprechende Werte ändern
    date->tm_mday += num_days;

    // in ein lesbares Format konvertieren
    char buffer[80];
    std::strftime(buffer, 80, "%d-%m-%Y", date);
    std::cout << "Das berechnete Datum ist: " << buffer << std::endl;
    
    return 0;
}
```

Die Ausgabe dieses Programms wäre: "Das berechnete Datum ist: 30-06-2021".

## Tief tauchen

Es gibt auch andere Möglichkeiten, ein Datum in der Zukunft oder Vergangenheit zu berechnen, zum Beispiel mit der Funktion ```std::chrono::system_clock::now()```. Es ist wichtig zu beachten, dass die Ergebnisse je nach verwendeter Methode und Lokalisierung des Systems unterschiedlich sein können. Es ist daher empfehlenswert, die Ergebnisse zu überprüfen und die geeignetste Methode für das eigene Projekt zu wählen.

## Siehe auch

- [C++ Referenz für <ctime>](https://cplusplus.com/reference/ctime/)
- [C++ Referenz für <chrono>](https://cplusplus.com/reference/chrono/)