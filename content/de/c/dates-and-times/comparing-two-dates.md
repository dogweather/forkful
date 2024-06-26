---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:53:26.395980-07:00
description: "Wie geht das: C hat keinen integrierten Typ f\xFCr Daten, was die Verwendung\
  \ der `time.h`-Bibliothek zur Arbeit mit Datum- und Zeitstrukturen notwendig\u2026"
lastmod: '2024-03-13T22:44:54.366206-06:00'
model: gpt-4-0125-preview
summary: "C hat keinen integrierten Typ f\xFCr Daten, was die Verwendung der `time.h`-Bibliothek\
  \ zur Arbeit mit Datum- und Zeitstrukturen notwendig macht."
title: Zwei Daten vergleichen
weight: 27
---

## Wie geht das:
C hat keinen integrierten Typ für Daten, was die Verwendung der `time.h`-Bibliothek zur Arbeit mit Datum- und Zeitstrukturen notwendig macht. Die `tm`-Struktur und die Funktion `difftime()` werden üblicherweise verwendet, um Daten zu vergleichen. Unten ist ein Beispiel, das zeigt, wie man zwei Daten vergleicht:

```c
#include <stdio.h>
#include <time.h>

int main() {
    struct tm date1 = {0};
    struct tm date2 = {0};
    double seconds;

    // Erstes Datum (JJJJ, MM, TT)
    date1.tm_year = 2023 - 1900; // Jahr seit 1900
    date1.tm_mon = 3 - 1;        // Monat [0-11]
    date1.tm_mday = 15;          // Tag des Monats [1-31]

    // Zweites Datum (JJJJ, MM, TT)
    date2.tm_year = 2023 - 1900;
    date2.tm_mon = 4 - 1;
    date2.tm_mday = 14;

    // Umwandeln in time_t-Format
    time_t time1 = mktime(&date1);
    time_t time2 = mktime(&date2);

    // Vergleichen
    seconds = difftime(time1, time2);

    if (seconds == 0) {
        printf("Die Daten sind identisch.\n");
    } else if (seconds > 0) {
        printf("Das erste Datum liegt nach dem zweiten Datum.\n");
    } else {
        printf("Das erste Datum liegt vor dem zweiten Datum.\n");
    }

    return 0;
}
```

Die Ausgabe könnte sein:

```text
Das erste Datum liegt vor dem zweiten Datum.
```

Dieses Programm initialisiert zwei `tm`-Strukturen mit spezifischen Daten, wandelt diese mit `mktime()` in das `time_t`-Format um und vergleicht sie schließlich mit `difftime()`, das die Differenz in Sekunden (als `double`) zwischen den beiden Zeiten zurückgibt.

## Vertiefung
In den Anfangstagen von C erforderten Datums- und Zeitoperationen manuelle Berechnungen, oft unter Berücksichtigung von Schaltjahren, der unterschiedlichen Anzahl von Tagen in den Monaten und sogar Schaltsekunden. Die Einführung von `time.h` im ANSI-C-Standard brachte eine Standardisierung der Zeitbehandlung in C, was die Datums- und Zeitoperationen vereinfachte.

Die Verwendung von `time.h` für den Datenvergleich ist unkompliziert, hat aber Einschränkungen. Die `tm`-Struktur berücksichtigt keine Zeitzonen oder Sommerzeit, und `difftime()` liefert nur die Differenz in Sekunden und fehlt an Feingranularität für bestimmte Anwendungen.

Für Anwendungen, die robustere Datum-Zeit-Operationen erfordern, einschließlich der Unterstützung für Zeitzonen, Übergänge der Sommerzeit und präzisere Zeitintervalle, bieten Bibliotheken wie `date.h` (eine Howard Hinnant-Datum-Bibliothek, kein Teil der Standardbibliothek) eine moderne Alternative zu `time.h`. Diese Bibliotheken bieten umfassendere Werkzeuge für Datum-Zeit-Manipulationen in C++, profitieren von Jahrzehnten der Evolution im Programmiersprachendesign. Für C-Programmierer bleibt das Nutzen dieser externen Bibliotheken oder das akribische Handhaben der Feinheiten von Datum-Zeit-Berechnungen direkt notwendig, um präzise und kulturell bewusste Datum-Zeit-Manipulationen zu erreichen.
