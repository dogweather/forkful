---
title:    "C: Ein Datum in der Zukunft oder Vergangenheit berechnen"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von Daten in der Zukunft oder Vergangenheit kann in der Programmierung nützlich sein, um zum Beispiel die Lieferzeit einer Bestellung zu bestimmen oder die Gültigkeit eines Angebots zu überprüfen.

## Wie

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, gibt es mehrere Schritte, die wir in C programmieren können. Zuerst müssen wir das aktuelle Datum erhalten, entweder durch die Verwendung der Funktion `time()` oder durch die Verwendung der `struct tm` Struktur. Dann können wir die `struct tm` Struktur manipulieren, indem wir Werte für Tag, Monat, Jahr und ggf. die Uhrzeit anpassen. Anschließend müssen wir die neue Struktur in ein lesbares Datum umwandeln, das wir dann auf dem Bildschirm ausgeben können.

```C
#include <stdio.h>
#include <time.h>

int main()
{
    // Aktuelles Datum erhalten
    time_t now = time(NULL);

    // struct tm Struktur erstellen
    struct tm *future_date;

    // Datumsänderung: 14 Tage in die Zukunft
    now += 14*24*60*60;

    // struct tm aktualisieren
    future_date = localtime(&now);

    // Lesbares Datum erzeugen
    char future_str[20];
    strftime(future_str, 20, "%d.%m.%Y", future_date);

    // Ausgabe des zukünftigen Datums
    printf("Das Datum in 14 Tagen wird sein: %s\n", future_str);

    return 0;
}
```

### Ausgabe:

```
Das Datum in 14 Tagen wird sein: 22.08.2021
```

## Deep Dive

Beim Berechnen von Datumswerten in der Zukunft oder Vergangenheit muss man beachten, dass einige Monate weniger als 31 Tage haben und dass es Schaltjahre gibt. Dies kann zu Inkonsistenzen führen, wenn man einfach nur Werte zu einem Datum addiert oder subtrahiert. Daher ist es wichtig, sich mit der Struktur der Datumsberechnung vertraut zu machen und geeignete Funktionen wie `mktime()` und `strftime()` zu verwenden. Auch die Verwendung von verschiedenen Zeitzonen kann zu Problemen führen, weshalb es wichtig ist, einheitliche Standards wie UTC zu verwenden.

## Siehe auch

- [Offizielle Dokumentation von strftime() (auf Deutsch)](https://www.gnu.org/software/libc/manual/html_node/Formatting-Calendar-Time.html)
- [Zeitzonen und deren Verwendung in Programmen (auf Deutsch)](https://de.wikipedia.org/wiki/Liste_von_Zeitzonen)
- [Programm zum Berechnen von Datumsdifferenzen (auf Deutsch)](https://www.gm-d.de/aktuelle-und-zukuenftige-datum-differenz-in-tagen-berechnen/)