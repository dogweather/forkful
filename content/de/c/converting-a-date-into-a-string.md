---
title:                "Ein Datum in einen String umwandeln"
date:                  2024-02-03T17:53:50.811772-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ein Datum in einen String umwandeln"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/converting-a-date-into-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Die Umwandlung eines Datums in einen String in C beinhaltet die Übersetzung einer Datenstruktur oder eines Zeitstempels in ein menschenlesbares Format. Programmierer führen diese Aufgabe oft durch, um Daten in Logs, Benutzeroberflächen oder beim Speichern von Daten in einem textbasierten Format wie JSON oder CSV anzuzeigen.

## Wie geht das:

Die Funktion `strftime` aus der Bibliothek `<time.h>` wird häufig für diesen Zweck verwendet. Sie ermöglicht es Ihnen, Datum und Zeit auf verschiedene Weisen zu formatieren, indem Formatierungsspezifizierer angegeben werden. Hier ist ein schnelles Beispiel:

```c
#include <stdio.h>
#include <time.h>

int main() {
    char dateStr[100];
    time_t now = time(NULL);
    struct tm *ptm = localtime(&now);

    // Datum & Zeit in String umwandeln (z.B. "Wed Jun 30 21:49:08 2021")
    strftime(dateStr, sizeof(dateStr), "%a %b %d %H:%M:%S %Y", ptm);
    
    printf("Aktuelles Datum und Uhrzeit: %s\n", dateStr);
    return 0;
}
```

Eine Beispiel-Ausgabe könnte so aussehen:

```
Aktuelles Datum und Uhrzeit: Wed Jun 30 21:49:08 2021
```

Sie können das Format anpassen, indem Sie die an `strftime` übergebenen Formatierungsspezifizierer ändern. Um das Datum beispielsweise im Format `JJJJ-MM-TT` zu erhalten, würden Sie `"%Y-%m-%d"` verwenden.

## Vertiefung

Die Funktion `strftime` und die Bibliothek `<time.h>` sind Teil der C-Standardbibliothek, die bis zum ursprünglichen ANSI C-Standard (C89/C90) zurückreicht. Während dieser Ansatz im Vergleich zu modernen Programmiersprachen, die intuitivere Datum- und Zeitbibliotheken anbieten, unkompliziert und auf vielen Plattformen unterstützt wird, kann er im Vergleich als niedrigstufig und umständlich erscheinen.

Man sollte jedoch beachten, dass die Zeitfunktionen der C-Standardbibliothek zwar weit verbreitet und relativ einfach zu verwenden sind, ihnen jedoch einige der komplexeren Zeitzone-Manipulations- und Internationalisierungsfunktionen fehlen, die in Bibliotheken neuerer Sprachen oder in Dritt-C-Bibliotheken wie International Components for Unicode (ICU) zu finden sind.

Dennoch machen die Anpassungsfähigkeit und die breite Plattformunterstützung der Funktion `strftime` sie zu einem zuverlässigen und nützlichen Werkzeug für die Umwandlung von Datumsstrings in C. Programmierer, die von Sprachen mit höherstufigen Datumsbibliotheken kommen, müssen sich vielleicht an ihre niedrigstufige Natur gewöhnen, werden sie aber für das Formatieren von Daten und Zeiten für eine Vielzahl von Anwendungen als bemerkenswert leistungsfähig und vielseitig empfinden.
