---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Elm: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Beim Programmieren ist es oft nötig, ein Datum in eine Zeichenfolge umzuwandeln. Das bedeutet, dass wir ein Datum, das wir als Objekt haben, in einen lesbaren Text verwandeln müssen. Das kann hilfreich sein, um das Datum auf einer Webseite anzuzeigen oder in einer Datei zu speichern.

## So geht's:
Um ein Datum in eine Zeichenfolge umzuwandeln, gibt es in Elm die `toString` Funktion. Diese Funktion nimmt ein Datum als Argument und gibt eine Zeichenfolge zurück, die das Datum in einem bestimmten Format enthält. Hier ist ein Beispiel:

```Elm
import Date exposing (Date)

 Date.fromCalendarDate 2021 05 20 |> Date.toString    --> "05/20/2021"
```

Man kann auch angeben, in welchem Format man die Zeichenfolge haben möchte, indem man der `toString` Funktion ein zweites Argument hinzufügt. Hier sind einige Beispiele:

```Elm
 Date.fromCalendarDate 2021 05 20 |> Date.toString "dd MMM yyyy"  --> "20 May 2021"
 Date.fromCalendarDate 2021 05 20 |> Date.toString "MM/dd/yy"     --> "05/20/21"
```

## Tiefer Einblick:
Die Funktion `toString` wird nicht nur in Elm, sondern auch in anderen Programmiersprachen häufig verwendet, da es notwendig ist, das Datum in verschiedene Formate umzuwandeln, je nachdem, wo es angezeigt wird. Es gibt viele Alternativen zu `toString`, wie z.B. die `strftime` Funktion in der Programmiersprache C. Auch in Elm gibt es noch andere Möglichkeiten, ein Datum als Zeichenfolge darzustellen, z.B. mit der `toTime` Funktion, die ein Datum als Zeitstempel in Millisekunden zurückgibt.

In der `toString` Funktion gibt es auch die Möglichkeit, ein benutzerdefiniertes Format anzugeben, indem man bestimmte Symbole verwendet. Zum Beispiel steht `yyyy` für das vierstellige Jahr, `MM` für die zweistellige Monatsangabe und `dd` für den Tag im Monat. Eine vollständige Liste mit allen verfügbaren Symbolen findet man in der Elm Dokumentation.

## Siehe auch:
- [Elm Dokumentation zu Datum und Zeit](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [Dokumentation zu strftime in C](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)
- [Alternative Möglichkeiten, ein Datum in Elm darzustellen](https://medium.com/@jamesmacaula/elm-dates-done-right-aa3ae559dbb8)