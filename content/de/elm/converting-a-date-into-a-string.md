---
title:                "Das Umwandeln eines Datums in eine Zeichenkette"
html_title:           "Elm: Das Umwandeln eines Datums in eine Zeichenkette"
simple_title:         "Das Umwandeln eines Datums in eine Zeichenkette"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Möchtest du ein Datum in ein lesbare Textform umwandeln? Vielleicht möchtest du dieses Datum in deiner Benutzeroberfläche anzeigen oder in einer JSON-Nachricht senden. In jedem Fall kannst du mit Elm ganz einfach ein Datum in eine Zeichenfolge konvertieren.

## Wie man es macht

Die Funktion `toString` in der Elm-Standardbibliothek kann verwendet werden, um ein Datum in ein lesbare Format umzuwandeln. Um es zu verwenden, müssen wir zunächst die `Date`-Bibliothek importieren. Dann können wir das gewünschte Datum als Argument an `toString` übergeben und das Ergebnis in einer Variablen speichern.

```elm
import Date exposing (Date)

convertedDate : String
convertedDate =
  toString (Date.fromCalendarDate 2021 10 20)
```

Das obenstehende Beispiel zeigt, wie das Datum 20.10.2021 in die ISO-8601-Datumsformat umgewandelt wird. Die Ausgabe sollte als `"2021-10-20"` sein.

Wir können auch benutzerdefinierte Formate für die Ausgabe angeben, indem wir die `format` Funktion anstelle von `toString` verwenden. Zum Beispiel:

```elm
import Date exposing (Date)
import Date.Format exposing (format)

convertedDate : String
convertedDate =
  format "%d.%m.%Y" (Date.fromCalendarDate 2021 10 20)
```

In diesem Fall wird das Datum im Format "20.10.2021" ausgegeben.

## Tiefere Einblicke

Die `toString` Funktion verwendet standardmäßig das ISO-8601-Datumsformat, das international anerkannt wird. Dies bedeutet, dass die Ausgabe immer das gleiche Format hat, unabhängig von der Region oder der Sprache des Benutzers. Dies ist besonders nützlich, wenn du internationale Benutzer hast, da du dir keine Gedanken über regionale Einstellungen machen musst.

Wenn du jedoch eine benutzerdefinierte Formatierung für die Ausgabe benötigst, solltest du die `format` Funktion verwenden. Mit dieser Funktion kannst du spezifische Symbole angeben, um das Datum in dem gewünschten Format anzuzeigen, einschließlich Wochentag, Monatsname, Jahr und mehr.

Insgesamt bietet Elm eine einfache und intuitive Möglichkeit, ein Datum in eine Zeichenfolge zu konvertieren. Neben `toString` und `format` gibt es auch Funktionen wie `toIso` und `toCalendarDate`, um ein Datum in andere unterstützte Formate zu konvertieren. Weitere Informationen findest du in der Elm Dokumentation für die `Date` und `Date.Format` Bibliotheken.

## Siehe auch

- [Elm Dokumentation für das Date-Modul](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [Elm Dokumentation für das Date.Format-Modul](https://package.elm-lang.org/packages/elm/time/latest/Date-Format)