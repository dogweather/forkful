---
title:                "Einen Datum aus einem String parsen"
html_title:           "Elixir: Einen Datum aus einem String parsen"
simple_title:         "Einen Datum aus einem String parsen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Parsen eines Datums aus einem String bezeichnet den Prozess, bei dem ein String in eine Datumsstruktur umgewandelt wird. Programmierer machen das, um eine menschenlesbare Datumsdarstellung in eine für die Maschine verarbeitbare Form zu bringen.

## So geht's:

Hier ist eine Code-Beispiel in Elm, wie man ein Datum in der Form von - Jahr-Monat-Tag - in ein tatsächliches Datum umwandeln kann:

```Elm
import Time

stringZuDatum : String -> Maybe Date
stringZuDatum s =
  case String.split "-" s of
      [ jahr, monat, tag ] ->
          Time.fromString (jahr ++ "T" ++ monat ++ "-" ++ tag )

      _ ->
          Nothing
```

Probieren wir es mit dem String "2022-03-30". Die Ausgabe sollte das Datum "30. März 2022" als `Date`-Objekt sein.

```Elm
main =
    Html.text <| case stringZuDatum "2022-03-30" of
        Just date ->
            toString date

        Nothing ->
            "Fehler beim Parsen des Datums"
```

## Tiefere Einblicke

Historisch gesehen, mussten Programmierer zunächst die einzelnen Teile des Datums (Tag, Monat, Jahr) aus dem String extrahieren und dann in Einzelteile umwandeln. Modernere Sprachen wie Elm bieten jedoch einfache Funktionen, die diesen Vorgang automatisieren und vereinfachen.

Als Alternative könnte man die Datumsteile auch mathematisch berechnen, dies eignet sich aber eher für Fälle, in denen man mit spezifischen Datumstypen und -formaten arbeitet. 

Die Implementierungsdetails variieren je nach Sprache und Framework. In Elm verwenden wir `Time.fromString`, die ein `String` annimmt und versucht, ihn in ein `Date` umzuwandeln, indem sie das Standard-ISO-8601-Datumsformat verwendet.

## Siehe auch

Weitere Informationen finden Sie in den offiziellen Elm-Dokumenten und in verwandten Tutorials:

- [Elm Time Paket](https://package.elm-lang.org/packages/elm/time/latest/)