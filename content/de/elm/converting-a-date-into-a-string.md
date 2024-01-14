---
title:                "Elm: Ein Datum in eine Zeichenkette umwandeln"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum
Das Konvertieren eines Datums in einen String ist ein häufiger Schritt beim Programmieren von Webanwendungen. Es ermöglicht es uns, Daten in einem spezifischen Format anzuzeigen, das für die Benutzer leichter verständlich ist.

## So geht's
Das Konvertieren eines Datums in einen String ist in Elm einfach, da die Sprache eine integrierte Funktion dafür besitzt. Wir verwenden die `toString` Funktion, um ein Datum in einen String zu konvertieren. Hier ist ein Beispielcode, der ein Datum in das amerikanische Datumsformat umwandelt:

```Elm
import Date exposing (..)
import Date.Format exposing (format)

date = Date.fromCalendarDate 2021 3 24
str = date |> format "mm/dd/yyyy"
-- 03/24/2021
```

In diesem Beispiel importieren wir das `Date` und `Date.Format` Modul und verwenden die `fromCalendarDate` Funktion, um ein Datum zu erstellen. Dann verwenden wir die `format` Funktion, um das Datum in das gewünschte Format zu konvertieren, in diesem Fall das amerikanische Datumsformat mit Monat/Tag/Jahr. Das Ergebnis wird in der Variable `str` gespeichert und kann dann in unserer Anwendung angezeigt werden.

## Tiefergehende Informationen
Die `format` Funktion akzeptiert einen zweiten Parameter, der das Format des Datums angibt. Dies ermöglicht es uns, das Datum in verschiedenen Formaten zu konvertieren, je nach unseren Anforderungen. Wir können auch die `Date` und `Date.Format` Module verwenden, um die Wochentage und Monate in verschiedenen Sprachen anzuzeigen. Weitere Informationen zu diesen Funktionen finden Sie in der Elm Dokumentation.

## Siehe auch
- [Elm Datum Dokumentation] (https://package.elm-lang.org/packages/elm/time/latest/)
- [Formatieren von Datumsangaben in Elm] (https://dev.to/kadysz/how-to-format-dates-in-elm-gfc)