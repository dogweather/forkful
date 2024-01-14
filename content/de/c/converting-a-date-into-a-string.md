---
title:    "C: Umwandlung eines Datums in einen String"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von Datums- und Zeitangaben in Zeichenfolgen ist eine grundlegende Aufgabe in der Programmierung. Es erlaubt uns, Daten in einem für Menschen lesbareren Format auszugeben.

## Wie funktioniert es?

Um ein Datum in eine Zeichenfolge zu konvertieren, müssen wir zuerst die Funktion `sprintf()` aus der Standardbibliothek `<stdio.h>` verwenden. Diese Funktion ermöglicht es uns, ein Zeichen für Zeichen in einen string zu schreiben. Das folgende Beispiel zeigt, wie wir `sprintf()` verwenden können, um das aktuelle Datum in einer Zeichenfolge auszugeben:

```C
#include <stdio.h>
#include <time.h>

int main(){
    // Aktuelles Datum und Uhrzeit abrufen
    time_t now;
    time(&now);

    // Speicher für eine Zeichenfolge von 50 Zeichen reservieren
    char date[50];

    // Datum in die Zeichenfolge schreiben
    sprintf(date, "Heute ist %s", ctime(&now));

    // Zeichenfolge ausgeben
    printf("%s", date);

    return 0;
}
```

Die Ausgabe dieses Codes würde in etwas ähnlichem aussehen wie:

```
Heute ist Sat Jun 20 15:33:28 2020
```

Wie du sehen kannst, gibt `ctime()` ein vorgefertigtes Datum und Uhrzeitformat zurück, das wir dann mit `sprintf()` in unsere eigene Zeichenfolge einfügen können.

## Tiefergehende Infos

Die Funktion `sprintf()` verwendet einen sogenannten Format-String, um zu definieren, wie die Daten in die Zeichenfolge geschrieben werden sollen. In dem oben gezeigten Beispiel haben wir `%s` verwendet, um die Zeit und das Datum von `ctime()` in unsere Zeichenfolge zu schreiben. Es gibt jedoch viele andere Platzhalter, die wir nutzen können, z.B. `%d` für eine Ganzzahl oder `%f` für eine Fließkommazahl.

Wenn du deine eigenen benutzerdefinierten Datumsformate erstellen möchtest, kannst du auch die Funktion `strftime()` aus der Standardbibliothek `<time.h>` verwenden. Diese Funktion arbeitet ähnlich wie `ctime()`, gibt aber ein Datum in einem von dir definierten Format zurück.

## Siehe auch

- [Die offizielle Dokumentation zu sprintf()](https://www.cplusplus.com/reference/cstdio/sprintf/)
- [Die offizielle Dokumentation zu strftime()](https://www.cplusplus.com/reference/ctime/strftime/)
- [Ein ausführlicher Artikel über das Formatieren von Datum und Uhrzeit in C](https://www.programiz.com/c-programming/c-date-time)

Vielen Dank fürs Lesen und ich hoffe, dieser Beitrag hat dir geholfen, mehr über das Konvertieren von Daten in Zeichenfolgen in C zu erfahren. Bis zum nächsten Mal!