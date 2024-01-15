---
title:                "Vergleich von zwei Daten"
html_title:           "C: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Wer in der Welt der Programmierung unterwegs ist, stößt oft auf die Herausforderung, zwei Zeitpunkte miteinander vergleichen zu müssen. Ob es um das Sortieren von Daten oder das Überprüfen von Zeitspannen geht, das Vergleichen von zwei Datumswerten ist ein häufiger Bestandteil des Programmieralltags. In diesem Artikel werden wir uns ansehen, wie man in C zwei Datumswerte vergleichen kann und warum es wichtig ist, diese Fähigkeit zu beherrschen.

## Wie geht das?

Das Vergleichen von zwei Datumswerten in C kann auf verschiedene Arten erfolgen, je nachdem welche Informationen und Funktionen einem zur Verfügung stehen. Hier sind zwei mögliche Ansätze:

```C
#include <stdio.h>
#include <time.h>

int main() {

    // Erstellen von zwei Variablen mit Datumswerten
    time_t date1 = time(NULL); // aktuelles Datum
    time_t date2 = 173744400; // 2. September 1975, 0 Uhr
    
    // Erstellen von Structs tm mit jeweiligen Datumswerten
    struct tm *ptr1 = localtime(&date1); 
    struct tm *ptr2 = localtime(&date2);
    
    // Vergleich der beiden Datumswerte
    if (mktime(ptr1) > mktime(ptr2)) {
        printf("Das erste Datum ist später als das zweite Datum\n");
    } else if (mktime(ptr1) < mktime(ptr2)) {
        printf("Das zweite Datum ist später als das erste Datum\n");
    } else {
        printf("Die beiden Datumswerte sind gleich\n");
    }
    
    return 0;
}
```

In diesem Beispiel werden die Daten mithilfe der "time" und "localtime" Funktionen in Variablen und Structs gespeichert. Daraufhin werden die Timestamps mithilfe der "mktime" Funktion verglichen und je nach Ergebnis eine Ausgabe auf der Konsole generiert.

Eine weitere Möglichkeit, zwei Datumswerte miteinander zu vergleichen, ist die Verwendung der Funktion "difftime". Diese gibt die Differenz in Sekunden zwischen zwei Datumswerten zurück. Hier ist ein Beispiel:

```C
#include <stdio.h>
#include <time.h>

int main() {

    // Erstellen von Variablen mit Datumswerten
    time_t date1 = time(NULL); // aktuelles Datum
    time_t date2 = 173744400; // 2. September 1975, 0 Uhr
    
    // Berechnung der Differenz in Sekunden
    double difference = difftime(date1, date2);
    
    // Ausgabe der Differenz in Sekunden
    printf("Die Differenz zwischen den beiden Datumswerten beträgt %.0f Sekunden\n", difference);

    return 0;
}
```

Beide Ansätze sind effektive Möglichkeiten, um zwei Datumswerte in C zu vergleichen. Es ist jedoch wichtig zu beachten, dass die Ergebnisse je nach verwendetem Ansatz geringfügig abweichen können.

## Tiefergehende Informationen

Bei der Verwendung von Structs tm und mktime kann es zu Problemen mit der Zeitzone oder der Sommerzeitumstellung kommen, da diese Funktionen auf lokale Zeiten basieren. Um dieses Problem zu vermeiden, können die Funktionen "gmtime" und "timegm" verwendet werden, die auf der Coordinated Universal Time (UTC) basieren.

Außerdem gibt "difftime" nur die Differenz in Sekunden zurück, was möglicherweise nicht ausreicht, um genau zu bestimmen, welches Datum später ist. Hier kann die Bibliothek "timelib" behilflich sein, die die Differenz in Tagen, Stunden, Minuten und Sekunden zurückgibt.

## Siehe auch

- [C Datumswerterfassung](https://de.wikibooks.org/wiki/C-Programmierung:_Datumswerterfassung)
- [C Standard-Bibliothek: Datums- und Zeitfunktionen](https://de.wikibooks.org/wiki/C-Programmierung:_Datum-und_Zeitfunktionen)
- [C-Befehlsübersicht: Zeit und Datum](https://en.wikibooks.org/wiki/C_Programming/C_Reference/Time_and_Date_Functions)