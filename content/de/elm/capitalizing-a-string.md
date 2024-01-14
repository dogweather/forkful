---
title:    "Elm: Zeichenfolge großschreiben"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Warum

Das Kapitalisieren von Strings ist eine grundlegende Funktion, die in vielen Programmiersprachen verwendet wird. In Elm ist es besonders nützlich, um Texte in einer bestimmten Form zu präsentieren oder zu vergleichen. In diesem Blog-Beitrag werden wir uns genauer ansehen, wie man Strings in Elm kapitalisiert.

## Wie geht das?

Um einen String in Elm zu kapitalisieren, gibt es zwei verschiedene Ansätze: entweder mit einer eingebauten Funktion oder selbst geschriebenem Code.

Der einfachste Weg ist die Verwendung der eingebauten Funktion `String.toUpper`, die einen String in Großbuchstaben umwandelt. Ein Beispiel:

```Elm
String.toUpper "hallo" -- gibt "HALLO" aus
```

Wenn du jedoch deine eigene Funktion zum Kapitalisieren von Strings schreiben möchtest, könntest du Folgendes verwenden:

```Elm
capString : String -> String
capString str =
    String.fromList (
        List.map (\c -> Char.toUpper c) (String.toList str)
    )

capString "hallo" -- gibt "HALLO" aus
```

Beide Ansätze liefern das gleiche Ergebnis, es ist also eine persönliche Präferenz, welchen du verwendest.

## Tiefer Einblick

Für eine genauere Betrachtung der Kapitalisierung von Strings in Elm ist es wichtig zu verstehen, dass Strings in Elm als Listen von Charakteren behandelt werden. Daher können wir die `List`- und `Char`-Module nutzen, um unsere String-Kapitalisierungs-Funktion zu schreiben.

Beispiel: In der Funktion `capString` konvertieren wir einen String in eine Liste von Charakteren mit `String.toList` und wenden dann `Char.toUpper` auf jeden dieser Charaktere an. Schließlich wenden wir `String.fromList` an, um die Liste der charaktere in einen kapitalisierten String zurückzuwandeln.

## Siehe Auch

Hier sind einige hilfreiche Links, um mehr über die Arbeit mit Strings in Elm zu erfahren:

- Die [String-Modul Dokumentation](https://package.elm-lang.org/packages/elm/core/latest/String)
- Ein [Tutorial zu String-Funktionen in Elm](https://guide.elm-lang.org/strings/) von der offiziellen Elm-Website
- Eine [Präsentation über String-Operationen in Elm von Richard Fochler](https://www.youtube.com/watch?v=F-k-x5vrR6I)
- Ein [Blog-Beitrag über Text-Verarbeitung in Elm von Luke Westby](https://lukeplant.me.uk/blog/posts/text-handling-in-elm/)