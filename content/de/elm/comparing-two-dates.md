---
title:                "Vergleich von zwei Daten"
html_title:           "C#: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Elm-Programmierung: Vergleich von zwei Daten

## Was & Warum?
Der Datumsvergleich prüft, ob ein Datum vor, nach oder gleich einem anderen ist. Programmierer nutzen ihn, um in Apps zeitbezogene Logik zu implementieren, z. B. To-Do-Listen, Kalender usw.

## Wie geht das?
Hier ist ein einfacher Code in Elm für den Vergleich zweier Daten:

```Elm
import Time exposing (..)

datum1 = toTime (millisToPosix 1618236900000)
datum2 = toTime (millisToPosix 1564617600000)

vergleich = compare datum1 datum2

	case vergleich of
		LT -> "Datum1 ist früher als Datum2"
		EQ -> "Datum1 ist gleich Datum2"
		GT -> "Datum1 ist später als Datum2"
```

Dieser Code wandelt Millisekunden in Datumsobjekte um und verwendet dann die Methode `compare` von Elm zur Überprüfung.

## Tiefere Informationen
Historischer Kontext: Elm hat seine Aktualisierungen von Zeit- und Datumsvergleichsfunktionen in neueren Versionen stetig verbessert. Frühere Versionen benötigten zusätzliche Pakete, um die heutigen nativen `Time`-Funktionen zu simulieren.

Alternativen: Es gibt Bibliotheken wie `elm-date-extra` und `justinmimbs/date`, die erweiterte Funktionen für Datumsoperationen bieten. Aber in den meisten Fällen genügt die grundlegende `Time`-Bibliothek.

Implementierungsdetails: Die `compare`-Funktion vergleicht das Unix-Timestamp (die Anzahl der Millisekunden seit dem 1. Januar 1970) der beiden Daten und gibt eine der drei Werte `LT`, `EQ` oder `GT` zurück, die für "Kleiner als", "Gleich" und "Größer als" stehen.

## Weitere Lektüre
- Elm Time Bibliothek (https://package.elm-lang.org/packages/elm/time/latest/)
- Elm Vergleichsfunktion (https://package.elm-lang.org/packages/elm/core/latest/Basics#compare)
- Elm Date Extra Bibliothek (https://package.elm-lang.org/packages/elm-community/elm-date-extra/latest/)
- Justin Mimbs' Datum Bibliothek (https://package.elm-lang.org/packages/justinmimbs/date/latest/)