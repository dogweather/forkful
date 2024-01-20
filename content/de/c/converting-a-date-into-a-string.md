---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Java: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Datum in String umwandeln (Konvertierung) bedeutet einfach, ein Datumsformat in eine lesbare Textzeichenfolge (String) umzuwandeln. Programmierer tun dies, um Daten einfacher in Benutzeroberflächen anzeigen zu können und um Datumsformate zwischen verschiedenen Plattformen kompatibel zu machen.

## So geht's:

Verwendung der `strftime()` Funktion in der `time.h` Bibliothek zur Datumsformatierung.

```C
#include <stdio.h>
#include <time.h>

int main() {
    char datestring[40];
    time_t zeit = time(NULL);
    strftime(datestring, 40, "%d-%m-%Y %H:%M:%S", localtime(&zeit));
    printf("%s\n", datestring);
    return 0;
}
```

Ausgabe könnte so aussehen:

```
20-12-2022 14:35:21
```

## Vertiefung:

Historisch gesehen stammt die Funktion `strftime()` aus der ANSI C-Bibliothek und ist eine relativ alte Methode zur Formatierung von Datum/Zeit-Strings. Sie ist jedoch nach wie vor ein wertvolles Werkzeug wegen ihrer Einfachheit und Kürze. 

Eine alternative Lösung bietet die POSIX `getdate()` Funktion. Sie analysiert eine String-Eingabe und konvertiert sie in ein `struct tm`. Doch dies kann etwas komplizierter sein.

Die Implementierung von `strftime()` intern verwendet eine Kombination aus Hilfe-Methoden, um das Datum in Segmente aufzuteilen (z.B. Tag, Monat, Jahr, Stunde, Minute, Sekunde), welche dann in das angegebene Ergebnisformat umformatiert werden.

## Siehe auch:

Hier einige Links für weitere Informationen und verwandte Themen:

- `strftime()` in der cplusplus.com Dokumentation: http://www.cplusplus.com/reference/ctime/strftime/
- `getdate()` in der GNU Dokumentation: https://www.gnu.org/software/libc/manual/html_node/Parsing-a-Date-or-Time.html
- Diskussion über die Formatierung von Datum und Uhrzeit in C auf StackOverflow: https://stackoverflow.com/questions/1442116/how-to-get-the-date-and-time-values-in-a-c-program