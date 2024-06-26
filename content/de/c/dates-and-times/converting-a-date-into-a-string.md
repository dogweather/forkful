---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:53:50.811772-07:00
description: "Wie geht das: Die Funktion `strftime` aus der Bibliothek `<time.h>`\
  \ wird h\xE4ufig f\xFCr diesen Zweck verwendet. Sie erm\xF6glicht es Ihnen, Datum\
  \ und Zeit auf\u2026"
lastmod: '2024-03-13T22:44:54.365134-06:00'
model: gpt-4-0125-preview
summary: "Die Funktion `strftime` aus der Bibliothek `<time.h>` wird h\xE4ufig f\xFC\
  r diesen Zweck verwendet."
title: Ein Datum in einen String umwandeln
weight: 28
---

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
