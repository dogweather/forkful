---
title:                "Extrahieren von Teilzeichenketten"
html_title:           "Elm: Extrahieren von Teilzeichenketten"
simple_title:         "Extrahieren von Teilzeichenketten"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Extrahieren von Teilstrings kann in vielen verschiedenen Situationen nützlich sein, z.B. beim Überprüfen von Eingaben oder beim Bearbeiten von Texten. In diesem Artikel werden wir uns ansehen, wie man Teilstrings in Elm extrahiert und einige Hinweise geben, wann es sinnvoll ist, dies zu tun.

## Wie geht's

Das Extrahieren von Teilstrings in Elm ist sehr einfach. Wir haben zwei Hauptfunktionen zur Verfügung: `String.slice` und `String.left`. Lass uns mit `String.slice` anfangen, die uns einen Teilstring aus einem gegebenen String zurückgibt. Hier ist ein Beispiel:

```Elm
String.slice 3 6 "Hallo Welt" -- gibt "lo " zurück
```

Wie du sehen kannst, müssen wir den Start- und Endindex des Teilstrings angeben, den wir extrahieren möchten. In diesem Fall haben wir "lo " als Ergebnis erhalten, da es sich um die Buchstaben von Index 3 bis 6 handelt.

Wenn du einfach die ersten n Buchstaben eines Strings extrahieren möchtest, kannst du die `String.left` Funktion verwenden. Sie nimmt nur zwei Argumente, die Anzahl der Buchstaben, die wir extrahieren möchten, und den ursprünglichen String. Ein Beispiel:

```Elm
String.left 4 "Hallo Welt" -- gibt "Hall" zurück
```

Beide Funktionen sind sehr nützlich, wenn wir Teilstrings aus einem String extrahieren möchten, aber es gibt noch ein paar weitere Details zu beachten.

## Tiefer Einblick

Die `String.slice` Funktion kann auch negative Indizes akzeptieren, was es uns ermöglicht, Teilstrings vom Ende des Originalstrings aus zu extrahieren. Zum Beispiel:

```Elm
String.slice -7 -1 "Hallo Welt" -- gibt "o Wel" zurück
```

Die `String.left` Funktion ist auch sehr praktisch, wenn wir einen Teil eines Strings ignorieren möchten. Wenn wir zum Beispiel alle Zeichen nach dem 4. Index in einem String entfernen möchten, können wir einfach `String.left 4` verwenden und den Rest ignorieren.

Es ist auch wichtig zu beachten, dass die erste Stelle in einem String den Index 0 hat, nicht 1. Das heißt, dass der erste Buchstabe eines Strings mit einem Index von 0 extrahiert wird, nicht mit einem Index von 1.

## Siehe auch

Weitere Informationen über die Verwendung von Strings in Elm findest du in der offiziellen Dokumentation [hier](https://guide.elm-lang.org/strings/). Du kannst auch unsere anderen Artikel über Elm-Syntax und Funktionen durchstöbern, um deine Elm-Fähigkeiten weiter zu verbessern. Viel Spaß beim Programmieren!