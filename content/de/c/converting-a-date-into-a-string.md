---
title:                "Umwandlung eines Datums in eine Zeichenfolge"
html_title:           "C: Umwandlung eines Datums in eine Zeichenfolge"
simple_title:         "Umwandlung eines Datums in eine Zeichenfolge"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Was & Warum?

Das Umwandeln von einem Datum in eine Zeichenfolge bedeutet, dass wir ein Datum in einem bestimmten Format darstellen möchten, das für Menschen leichter lesbar ist. Die Programmiere dieses Verfahren, um es einfacher zu machen, mit Datumsangaben zu arbeiten.

# Wie geht das?

Wir können dieses Verfahren in C auf verschiedene Weise durchführen. Hier sind zwei Beispiele, die ein Datum und eine Uhrzeit in ein lesbare Zeichenfolge umwandeln:

```C
#include <stdio.h>
#include <time.h> //für die Zeitfunktionen

int main()
{
    time_t now = time(NULL); //aktuelle Zeit in Sekunden seit 1970
    // erstellen einer struct tm mit der aktuellen Zeit
    struct tm *timeinfo = localtime(&now);
    char buffer[100]; //eine Zeichenfolge für die Ausgabe
    //das Datum und die Uhrzeit in einem bestimmt Format ausgeben
    strftime(buffer, sizeof buffer, "%d.%m.%y %H:%M:%S", timeinfo);
    printf("Das aktuelle Datum und die Uhrzeit sind: %s\n", buffer);
    return 0;
}
```

Die Ausgabe wird in etwa so aussehen:

```
Das aktuelle Datum und die Uhrzeit sind: 28.06.20 13:26:47
```

Eine andere Möglichkeit, ein Datum in eine Zeichenfolge umzuwandeln, ist die Verwendung der Funktion `sprintf`. Hier ist ein Beispiel, das das gleiche Ergebnis wie das obere Beispiel liefert:

```C
#include <stdio.h>
#include <time.h>

int main()
{
    time_t now = time(NULL);
    struct tm *timeinfo = localtime(&now);
    char buffer[100];
    //das Datum und die Uhrzeit in einem bestimmt Format in die char-Array schreiben
    sprintf(buffer, "%02d.%02d.%02d %02d:%02d:%02d", timeinfo->tm_mday, timeinfo->tm_mon+1, timeinfo->tm_year+1900, timeinfo->tm_hour, timeinfo->tm_min, timeinfo->tm_sec);
    printf("Das aktuelle Datum und die Uhrzeit sind: %s\n", buffer);
    return 0;
}
```

# Tiefer gehen

Das Konzept, ein Datum in eine Zeichenfolge umzuwandeln, geht zurück bis in die Anfänge der Programmierung. Früher wurde dafür häufig die Funktion `sprintf` verwendet, jedoch ist die Verwendung von `strftime` heutzutage verbreiteter. Es gibt auch verschiedene andere Methoden, um das gewünschte Ergebnis zu erzielen, wie zum Beispiel die Verwendung von `sscanf` und `asctime`.

Um die erzeugte Zeichenfolge noch weiter anzupassen, können wir das Format in `strftime` ändern und verschiedene Optionen für das Ausgabedatum und die Uhrzeit verwenden. Hier sind einige Beispiele dafür, was geändert werden kann:

- `%d` steht für die Tagzahl (01-31)
- `%m` steht für den Monat (01-12)
- `%y` steht für das Jahr (00-99)
- `%H` steht für die Stunden im 24-Stunden-Format (00-23)
- `%M` steht für die Minuten (00-59)
- `%S` steht für die Sekunden (00-60)

Es gibt noch viele weitere Optionen und auch die Möglichkeit, Text in die Zeichenfolge einzufügen. Eine vollständige Liste der Optionen findet man in der C-Dokumentation.

# Siehe auch

Weitere Informationen und Beispiele finden Sie in der [offiziellen C-Dokumentation zu `strftime`](https://devdocs.io/c/ctime/strftime). Sie können auch nach Alternativen suchen, wie zum Beispiel die Verwendung von Bibliotheken wie `stdio.h` oder `boost::date_time`.