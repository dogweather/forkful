---
title:    "Elm: Umwandlung eines Datums in einen String"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Umwandeln von Daten in eine Zeichenkette ist ein grundlegender Schritt bei der Entwicklung von Webanwendungen. Mit dieser Fähigkeit können wir Datumsangaben auf einfache Weise darstellen und sie für unsere Benutzer leicht lesbar machen.

## Wie Geht's

Um ein Datum in eine Zeichenkette zu konvertieren, müssen wir die Funktion `toString` verwenden. Diese Funktion nimmt einen `Date`-Wert entgegen und gibt eine String-Repräsentation des Datums zurück. Hier ist ein Beispiel:

```Elm
toString (Date.fromTime 0)
```

Dies würde den String `"Jan 1, 1970"` zurückgeben, da der übergebene `Date`-Wert 0 Millisekunden seit dem 1. Januar 1970 entspricht.

Eine weitere nützliche Funktion ist `fromIsoString`, die es uns ermöglicht, ein Datum aus einer Zeichenkette zu erstellen, die dem ISO-8601-Format entspricht. Hier ist ein Beispiel:

```Elm
Date.fromIsoString "2019-06-05"
```

Dies würde den `Date`-Wert `Ok (Date.fromTime 1559702400000)` zurückgeben.

## Tiefentauchen

Es ist wichtig zu beachten, dass die Funktionen `toString` und `fromIsoString` standardmäßig im UTC-Format arbeiten. Wenn wir jedoch möglicherweise verschiedene Zeitzonen berücksichtigen müssen, können wir die `Time`-Bibliothek nutzen. Mit dieser Bibliothek können wir Daten in eine bestimmte Zeitzone konvertieren und auch die aktuelle Zeitzone des Benutzers abrufen.

Hier ist ein Beispiel, in dem wir die aktuelle Zeitzone des Benutzers nutzen, um ein Datum in eine Zeichenkette im entsprechenden Format zu konvertieren:

```Elm
import Time exposing (..)
import Time.Format exposing (..)

zoneDate : Date -> String
zoneDate date =
    Time.format "%d-%m-%Y" (convert (zoneFor date) date)

zoneDate (fromIsoString "2019-06-05")
```

Dies würde beispielsweise in der amerikanischen Zeitzone `"06-05-2019"` zurückgeben, aber in der europäischen Zeitzone `"05-06-2019"`.

## Siehe auch

- [Elm Dokumentation zu Dates](https://package.elm-lang.org/packages/elm-lang/core/latest/Date)
- [Time Bibliothek](https://package.elm-lang.org/packages/elm/time/latest/)
- [Time Format Bibliothek](https://package.elm-lang.org/packages/elm/time/latest/Time-Format)