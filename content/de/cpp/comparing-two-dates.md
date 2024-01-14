---
title:                "C++: Vergleich von zwei Daten"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Warum

Das Vergleichen von zwei Daten kann in der Programmierung sehr hilfreich sein, um beispielsweise festzustellen, welches Datum früher oder später liegt. Dies ist besonders nützlich, wenn man mit Anmeldedaten oder Buchungsdaten arbeitet.

# Wie geht man vor?

Um zwei Daten in C++ zu vergleichen, gibt es verschiedene Ansätze. Einer davon ist die Verwendung der eingebauten Funktion "difftime", die die Differenz zwischen zwei Zeitwerten in Sekunden zurückgibt. Schauen wir uns dazu einen Code-Beispiel an:

```C++
#include <iostream> 
#include <ctime> 
  
using namespace std; 
  
int main() 
{ 
    // Erstes Datum erstellen 
    tm date1 = { 0 }; 
    date1.tm_year = 120; // Jahr: 2020 
    date1.tm_mon = 4; // Monat: Mai
    date1.tm_mday = 5; // Tag: 5
  
    // Zweites Datum erstellen 
    tm date2 = { 0 }; 
    date2.tm_year = 120; // Jahr: 2020 
    date2.tm_mon = 4; // Monat: Mai
    date2.tm_mday = 8; // Tag: 8
  
    // Differenz berechnen 
    double difference = difftime(mktime(&date2), mktime(&date1)); 
  
    // Ausgabe
    cout << "Die Differenz in Sekunden zwischen den beiden Daten beträgt: " << difference << endl; 
  
    return 0; 
} 
```

**Ausgabe:**

Die Differenz in Sekunden zwischen den beiden Daten beträgt: 259200

In diesem Beispiel wurden zwei Daten erstellt und die Differenz in Sekunden zwischen ihnen berechnet. Man kann die Differenz nun natürlich noch in andere Zeiteinheiten, wie beispielsweise Tage oder Stunden, umrechnen.

# Tiefer tauchen

Wenn man sich genauer mit dem Vergleichen von zwei Daten beschäftigen möchte, muss man auch die verschiedenen Datentypen in Betracht ziehen. Zum Beispiel ist es nicht sinnvoll, zwei Daten vom Typ "string" miteinander zu vergleichen, da hierbei nur die ASCII-Werte verglichen werden. Auch muss man bei der Verwendung von Datumswerten aus Datenbanken darauf achten, dass diese im richtigen Datentyp vorliegen.

# Siehe auch

- [C++ Referenz für date.h](https://en.cppreference.com/w/cpp/header/datetime)
- [Tutorial: Arbeiten mit Datum und Uhrzeit in C++](https://www.learncpp.com/cpp-tutorial/513-0-binary-date-and-time/)