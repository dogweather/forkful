---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:38.863862-07:00
description: "Wie: In C bietet der Header `<time.h>` die notwendigen Funktionen und\
  \ Typen, um mit Daten und Zeiten zu arbeiten. Die Funktion `time()` ruft die aktuelle\u2026"
lastmod: '2024-03-13T22:44:54.363900-06:00'
model: gpt-4-0125-preview
summary: In C bietet der Header `<time.h>` die notwendigen Funktionen und Typen, um
  mit Daten und Zeiten zu arbeiten.
title: Das aktuelle Datum abrufen
weight: 29
---

## Wie:
In C bietet der Header `<time.h>` die notwendigen Funktionen und Typen, um mit Daten und Zeiten zu arbeiten. Die Funktion `time()` ruft die aktuelle Zeit ab, während `localtime()` diese Zeit in die lokale Zeitzone umrechnet. Um das Datum anzuzeigen, verwenden wir `strftime()`, um es als Zeichenkette zu formatieren.

Hier ist ein einfaches Beispiel:

```c
#include <stdio.h>
#include <time.h>

int main() {
    char puffer[80];
    time_t rohzeit;
    struct tm *zeitinfo;

    // Die aktuelle Zeit abrufen
    time(&rohzeit);
    // Sie in die lokale Zeit umrechnen
    zeitinfo = localtime(&rohzeit);
    
    // Das Datum formatieren und ausgeben
    strftime(puffer, 80, "Das heutige Datum ist %Y-%m-%d", zeitinfo);
    printf("%s\n", puffer);

    return 0;
}
```

Eine beispielhafte Ausgabe könnte so aussehen:

```
Das heutige Datum ist 2023-04-12
```

## Tiefgang
Die Zeitverwaltung in C, wie sie durch `<time.h>` ermöglicht wird, reicht zurück bis in die frühesten Tage der Sprache und der UNIX-Systeme. Sie basiert auf dem Datentyp `time_t`, der die aktuelle Zeit als Anzahl der Sekunden seit der Unix-Epoche (1. Januar 1970) darstellt. Obwohl dies effizient und universell kompatibel ist, bedeutet dies auch, dass die Zeitfunktionen der Standard-C-Bibliothek von dem Bereich und der Auflösung von `time_t` inhärent begrenzt sind.

Moderne Anwendungen, insbesondere solche, die hochauflösende Zeitstempel benötigen oder mit Daten weit in der Zukunft oder Vergangenheit umgehen müssen, können diese Einschränkungen als herausfordernd empfinden. Das Jahr-2038-Problem ist beispielsweise eine bekannte Illustration, bei der Systeme, die einen 32-Bit-`time_t` verwenden, überlaufen werden.

Für komplexere Zeit- und Datumsverwaltungen wenden sich viele Programmierer an externe Bibliotheken oder die von dem Betriebssystem bereitgestellten Funktionalitäten. In C++ bietet beispielsweise die Bibliothek `<chrono>` präzisere und vielseitigere Zeitmanipulationsmöglichkeiten.

Trotz seiner Einschränkungen macht die Einfachheit und Ubiquität von C's Zeitfunktionen sie für viele Anwendungen perfekt geeignet. Diese Werkzeuge zu verstehen, ist grundlegend für C-Programmierer, und bietet eine Mischung aus historischem Programmierkontext und praktischem, alltäglichem Nutzen.
