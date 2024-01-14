---
title:    "C++: Vergleich von zwei Daten"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Daten kann in zahlreichen Situationen nützlich sein, wie beispielsweise bei der Überprüfung von Aufgaben- oder Terminfristen, beim Filtern von Daten oder bei der Sortierung von Einträgen nach chronologischer Reihenfolge. In diesem Blog-Beitrag werden wir zeigen, wie man mithilfe von C++ zwei Daten miteinander vergleichen kann.

## Wie geht man vor

Um zwei Daten in C++ zu vergleichen, müssen diese in ein einheitliches Format umgewandelt werden, nämlich in ein Datum-Objekt. Dies kann mithilfe der <ctime> Bibliothek und der Funktionen `mktime` und `localtime` erreicht werden. Im Folgenden sehen Sie ein Beispielcode:

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main() {
  // Datum-Objekte erstellen
  struct tm date1 = {0}, date2 = {0};
  
  // Date1 setzen auf 10. Januar 2020
  date1.tm_year = 120;
  date1.tm_mon = 0;
  date1.tm_mday = 10;

  // Date2 setzen auf 15. Januar 2020
  date2.tm_year = 120;
  date2.tm_mon = 0;
  date2.tm_mday = 15;

  // Datum-Objekte in Zeitstempel umwandeln
  time_t time1 = mktime(&date1);
  time_t time2 = mktime(&date2);

  // Daten vergleichen und Ausgabe je nach Ergebnis
  if (time1 < time2) {
    cout << "Date1 liegt vor Date2" << endl;
  } else if (time1 > time2) {
    cout << "Date2 liegt vor Date1" << endl;
  } else {
    cout << "Die Daten sind gleich" << endl;
  }

  return 0;
}
```

Die Ausgabe dieses Codes ist:

```
Date1 liegt vor Date2
```

## Tiefere Einblicke

Bei der Umwandlung von Datum-Objekten in Zeitstempel ist es wichtig zu beachten, dass der Monatswert von 0 (Januar) bis 11 (Dezember) angegeben werden muss. Außerdem kann die Funktion `mktime` nur Daten bis zum Jahr 2038 verarbeiten. Für Daten nach 2038 muss ein anderer Ansatz gewählt werden.

Eine weitere wichtige Sache zu beachten ist, dass Datum-Objekte keine Informationen über die Zeitzone enthalten. Daher sollten alle Daten auf die Zeitzone UTC (Universal Time Coordinated) umgerechnet werden, bevor sie verglichen werden.

## Siehe auch

- [C++ Referenz für <ctime> Bibliothek](https://www.cplusplus.com/reference/ctime/)
- [Tutorial: Arbeiten mit Datums- und Zeitangaben in C++](https://www.moderncplusplus.com/cplusplus-dates-times/)