---
title:                "Das aktuelle Datum erhalten"
html_title:           "Elm: Das aktuelle Datum erhalten"
simple_title:         "Das aktuelle Datum erhalten"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Abrufen des aktuellen Datums ist eine häufige Aufgabe in der Programmierung. Es ermöglicht Programmierern, das aktuelle Datum in ihre Anwendung einzubinden und es für verschiedene Zwecke zu verwenden, wie z.B. für die Datumsangabe in Benutzeroberflächen, für die Berechnung von Zeiträumen oder für das Speichern von Dateien mit einem Datumsstempel.

## Wie geht's:
### Elm 0.19
```
Elm.Date.today
```
*Ausgabe:*
```
Ok (Date 2021 3 14)
```

### Elm 0.18
```
import Time exposing (now)
import Time.Date as Date

Date.fromTime <| now Time.utc

```
*Ausgabe:*
```
{ year = 2021, month = 3, day = 14 }
```

## Tiefen-Eintauchen:
Das Abrufen des aktuellen Datums ist eine Aufgabe, die in verschiedenen Programmiersprachen auf unterschiedliche Weise gelöst werden kann. In Elm 0.19 gibt es eine neue `Date`-Bibliothek, die das Manipulieren und Berechnen von Datumsangaben erleichtert. In älteren Versionen von Elm kann das aktuelle Datum mit einem Modul aus dem Time-Paket abgerufen werden. Alternativ können einige externe Bibliotheken verwendet werden, die erweiterte Funktionen für das Arbeiten mit Datumsangaben bieten.

## Siehe auch:
- [Elm-Datei-Bibliothek](https://package.elm-lang.org/packages/elm/date/latest/)
- [Elm-Zeitpaket](https://package.elm-lang.org/packages/elm/time/latest/)
- [Elm-Timelord-Bibliothek](https://package.elm-lang.org/packages/tesk9/elm-timelord/latest/)