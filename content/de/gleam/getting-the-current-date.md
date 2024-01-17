---
title:                "Das aktuelle Datum abrufen."
html_title:           "Gleam: Das aktuelle Datum abrufen."
simple_title:         "Das aktuelle Datum abrufen."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Abrufen des aktuellen Datums ist ein grundlegender und wichtiger Teil der Programmierung. Es ermöglicht es uns, Datum und Uhrzeit in unseren Anwendungen auf einfache und effiziente Weise zu verarbeiten. Programmierer verwenden diese Funktion oft für die Reihenfolge von Ereignissen, das Verwalten von Planungen oder das Berechnen von Ausführungszeiten.

## So geht's:

### Gleam-Syntax:

```Gleam
import DateTime

let current_date = DateTime.now()

IO.print("Das aktuelle Datum ist: {}", [DateTime.format(current_date, "%Y-%m-%d")])
```

### Ausgabe:

Das aktuelle Datum ist: 2021-09-09

## Tief einsteigen:

### Historischer Kontext:

Das Abrufen des aktuellen Datums ist ein wesentlicher Bestandteil der Programmierung seit den Anfängen der Computer. In frühen Programmiersprachen wie BASIC und FORTRAN war es jedoch aufgrund fehlender Bibliotheken und Funktionen deutlich komplexer.

### Alternativen:

Es gibt verschiedene Möglichkeiten, das aktuelle Datum in einer Programmiersprache zu erhalten. Einige Programmiersprachen haben eine integrierte Funktion, andere benötigen spezielle Bibliotheken oder Module, wie es bei Gleam der Fall ist.

### Implementation:

In Gleam wird das aktuelle Datum mithilfe der DateTime-Bibliothek abgerufen. Diese Bibliothek bietet verschiedene Funktionen für die Verarbeitung von Datum und Uhrzeit. Die `now()` Funktion gibt das aktuelle Datum im Format `datetime` zurück, welches dann mithilfe der `format()` Funktion in das gewünschte Format umgewandelt werden kann.

## Siehe auch:

- [Gleam DateTime Dokumentation](https://gleam.run/libraries/date-time/)
- [Erläuterungen zum aktuellen Datum in anderen Programmiersprachen](https://www.guru99.com/dates-date-times-in-programming.html)
- [Verwendung von Datum und Uhrzeit in Gleam-Anwendungen](https://medium.com/gleam-lang-blog/working-with-dates-and-times-in-gleam-2bb543b14c1f)