---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:00:01.464406-07:00
description: "Wie: C bietet keine eingebaute M\xF6glichkeit, direkt aus Strings Daten\
  \ zu parsen, deshalb greifen wir oft auf die `strptime` Funktion zur\xFCck, die\
  \ in der\u2026"
lastmod: '2024-03-13T22:44:54.362805-06:00'
model: gpt-4-0125-preview
summary: "C bietet keine eingebaute M\xF6glichkeit, direkt aus Strings Daten zu parsen,\
  \ deshalb greifen wir oft auf die `strptime` Funktion zur\xFCck, die in der `<time.h>`\
  \ Bibliothek f\xFCr POSIX-Systeme verf\xFCgbar ist."
title: Ein Datum aus einem String interpretieren
weight: 30
---

## Wie:
C bietet keine eingebaute Möglichkeit, direkt aus Strings Daten zu parsen, deshalb greifen wir oft auf die `strptime` Funktion zurück, die in der `<time.h>` Bibliothek für POSIX-Systeme verfügbar ist. Diese Funktion ermöglicht es uns, das erwartete Format des Eingabestrings zu spezifizieren und ihn in ein `struct tm` zu parsen, welches das Kalenderdatum und die Zeit in ihre Komponenten zerlegt darstellt.

Hier ist ein einfaches Beispiel, wie man `strptime` nutzt, um ein Datum aus einem String zu parsen:

```c
#include <time.h>
#include <stdio.h>

int main() {
    const char *dateStr = "2023-04-01";
    struct tm tm;
    char buf[255];

    // Das Datumsstring in struct tm parsen
    if (strptime(dateStr, "%Y-%m-%d", &tm) == NULL) {
        printf("Fehler beim Parsen des Datums.\n");
    } else {
        // Verwendung von strftime, um das Datum in einem lesbaren Format auszugeben
        strftime(buf, sizeof(buf), "%A, %B %d, %Y", &tm);
        printf("Geparstes Datum: %s\n", buf);
    }

    return 0;
}
```

Die Ausgabe dieses Programms wäre:

```
Geparstes Datum: Samstag, April 01, 2023
```

Es ist essenziell, potenzielle Fehler zu behandeln, wie zum Beispiel, wenn `strptime` das Muster nicht abgleichen kann oder unerwartete Eingaben begegnet.

## Tiefer gehend
Die `strptime` Funktion, obwohl mächtig, ist kein Teil der standard C-Bibliothek und findet sich hauptsächlich auf POSIX-konformen Systemen wie Linux und UNIX. Diese Einschränkung bedeutet, dass Programme, die auf `strptime` zum Parsen von Daten aus Strings angewiesen sind, möglicherweise nicht auf nicht-POSIX-Systeme wie Windows portierbar sind, ohne zusätzliche Kompatibilitätsschichten oder Bibliotheken.

Historisch gesehen erforderte das Handhaben von Daten und Zeiten in C viel manuelle Manipulation und Sorgfalt, besonders unter Berücksichtigung verschiedener Lokalitäten und Zeitzonen. Moderne Alternativen und Erweiterungen zu C, wie die C++ `<chrono>` Bibliothek und Dritt-Bibliotheken wie Howard Hinnants Date-Bibliothek für C++, bieten robustere Lösungen für die Manipulation von Daten und Zeiten, inklusive Parsing. Diese Bibliotheken bieten typischerweise bessere Unterstützung für eine breitere Palette von Datumsformaten, Zeitzonen und Fehlerbehandlungsmechanismen, was sie für neue Projekte, die umfangreiche Fähigkeiten zur Manipulation von Datum und Zeit erfordern, vorzuziehen macht.

Dennoch kann das Verständnis, wie man Daten aus Strings in C parst, vorteilhaft sein, besonders beim Arbeiten an oder beim Warten von Projekten, die mit Systemen kompatibel sein müssen, wo diese modernen Werkzeuge nicht verfügbar sind, oder beim Arbeiten innerhalb der Beschränkungen von strikten C-Programmierumgebungen.
