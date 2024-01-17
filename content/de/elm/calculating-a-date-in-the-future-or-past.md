---
title:                "Ein Datum in der Zukunft oder Vergangenheit berechnen"
html_title:           "Elm: Ein Datum in der Zukunft oder Vergangenheit berechnen"
simple_title:         "Ein Datum in der Zukunft oder Vergangenheit berechnen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Berechnung von Datumsangaben in der Vergangenheit oder Zukunft ist eine häufige Aufgabe in der Programmierung. Sie ermöglicht es uns, zeitbasierte Funktionen in unseren Programmen zu implementieren, wie z.B. die Planung von Aufgaben oder Erinnerungen. 

## Wie geht's?

Eine Möglichkeit, ein Datum in der Zukunft oder Vergangenheit zu berechnen, ist die Verwendung des in Elm integrierten Moduls ```Date``` (Datum). Dieses Modul bietet verschiedene Funktionen, die es uns ermöglichen, Datumsangaben zu manipulieren und berechnen. Schauen wir uns ein Beispiel an:

```
import Date exposing (..)

-- Berechnung eines Datums 10 Tage in der Zukunft
futureDate = add (days 10) today

-- Ausgabe: 2021-09-24
```

Wir importieren das ```Date``` Modul und nutzen dann die Funktion ```add```, um 10 Tage zu unserem aktuellen Datum (```today```) hinzuzufügen. Dies ergibt das gewünschte Datum in der Zukunft. Dem ```add``` Befehl können auch andere Zeitangaben wie Monate oder Jahre hinzugefügt werden.

Ebenso können wir auch in die Vergangenheit mit der Funktion ```subtract``` gehen und eine bestimmte Anzahl an Tagen, Monaten oder Jahren von einem bestimmten Datum abziehen.

```
import Date exposing (..)

-- Berechnung eines Datums 1 Monat in der Vergangenheit
pastDate = subtract (months 1) "2021-09-24"

-- Ausgabe: 2021-08-24
```

## Tiefer Einblick

Die Berechnung von Datumsangaben hat eine lange Geschichte in der Programmierung. Früher musste diese Funktion manuell implementiert werden, was zu komplexem und fehleranfälligem Code führte. Heutzutage bieten viele Programmiersprachen, einschließlich Elm, integrierte Module oder Bibliotheken, die diese Aufgabe erleichtern.

Alternativ können wir auch die Zeitstempel nutzen, die in Unix-basierten Systemen verwendet werden, um Zeit zu berechnen. Dabei wird die Anzahl der Sekunden seit dem 1. Januar 1970 verwendet, um ein bestimmtes Datum zu repräsentieren. Diese Methode ist zwar nicht so intuitiv wie die Nutzung von Datum-Objekten, aber in bestimmten Situationen kann sie von Vorteil sein.

Bei der Implementierung einer Datumsberechnung ist es wichtig, alle möglichen Ausnahmefälle, wie z.B. Schaltjahre oder unterschiedliche Anzahl von Tagen in einem Monat, zu berücksichtigen. Das ```Date``` Modul in Elm erledigt dies bereits für uns, was uns viel Zeit und Mühe spart.

## Siehe auch

- Elm Dokumentation für ```Date``` Module: https://package.elm-lang.org/packages/elm/time/latest/Date