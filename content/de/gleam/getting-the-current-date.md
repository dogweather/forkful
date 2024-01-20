---
title:                "Das aktuelle Datum abrufen"
html_title:           "Gleam: Das aktuelle Datum abrufen"
simple_title:         "Das aktuelle Datum abrufen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Abrufen des aktuellen Datums ist eine Standardfunktion in der Programmierung. Es wird oft von Programmierern verwendet, um Zeitstempel für Benutzeraktivitäten zu erstellen oder Termine und Fristen zu verwalten.

## So geht's:

`scheduler` von Gleam enthält die Möglichkeit, Datum und Uhrzeit abzurufen. Hier ist ein Beispiel, wie es aussieht:

```gleam
import gleam/otp/scheduler.{now}

pub fn current_date() {
  let timestamp = now(Native)
  let {_year, _month, _day} = timestamp.date()

  println("Jahr: {year}, Monat: {month}, Tag: {day}")
}
```

Dieser Code gibt das aktuelle Jahr, Monat und Tag zurück. 

## Tief tauchen:

Früher musste man die Uhrzeit und das Datum manuell vom Betriebssystem abrufen und umwandeln, was unzuverlässig und ineffizient war. Gleam hat diesen Prozess durch die Einbeziehung von eingebauten Funktionen vereinfacht, die Zeit und Datum in einer viel mehr handhabbaren Weise abrufen.

Ein alternativer Ansatz zum Abrufen des aktuellen Datums besteht darin, auf interne Funktionen von Gleam zu verzichten und stattdessen eine Drittanbieter-Bibliothek zu verwenden. Obwohl dies zusätzliche Funktionen bereitstellen könnte, erhöht es auch die Komplexität und möglicherweise die Unzuverlässigkeit des Programms.

In Bezug auf die Implementierungsdetails extrahiert die `now` Funktion von Gleam das aktuelle Datum und die aktuelle Uhrzeit aus dem internen Managementsystem. Diese Information wird dann in ein leicht zugängliches Format umgewandelt, das für weitere Manipulationen oder Ausgaben genutzt werden kann.

## Siehe auch:

Für weitere Informationen über Gleam und seine Funktionen, besuchen Sie:

1. [Die offizielle Gleam Dokumentation](https://gleam.run/docs/)
2. [Gleam Repositorium auf Github](https://github.com/gleam-lang/gleam)