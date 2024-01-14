---
title:    "Elm: Löschen von Zeichen mit passendem Muster"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte man sich dafür interessieren, Zeichen zu löschen, die einem bestimmten Muster entsprechen? Nun, es gibt verschiedene Gründe dafür. Möglicherweise möchtest du deine Datenbereinigung verbessern oder einfach nur ein Problem in deinem Code lösen. In jedem Fall kann das Löschen von Zeichen basierend auf einem Muster äußerst nützlich sein.

## So geht's
Um Zeichen zu löschen, die einem bestimmten Muster entsprechen, können wir die `String.filter` Funktion von Elm verwenden. Sie akzeptiert eine Funktion als Eingabe und wendet sie auf jeden Buchstaben im String an. Wenn die Funktion `True` zurückgibt, wird der Buchstabe beibehalten, ansonsten wird er gelöscht.

 ``` Elm
 let myString = "Elm ist eine großartige Sprache"
 String.filter (\char -> not (char == 'e' || char == 'a')) myString
 -- gibt "Elm ist in großrtige Sprche" aus
 ```

In diesem Beispiel verwenden wir die anonyme Funktion `(\char -> not (char == 'e' || char == 'a'))`, die `True` für alle Buchstaben zurückgibt, die nicht gleich 'e' oder 'a' sind. Alle anderen werden gelöscht. Das Ergebnis ist ein neuer String ohne die angegebenen Buchstaben.

## Tiefergehende Informationen
Es kann hilfreich sein, die Funktion `String.filter` mit regulären Ausdrücken zu kombinieren, um komplexere Muster zu identifizieren und zu löschen. Elm bietet die `Regex`-Bibliothek, die verschiedene Funktionen und Operatoren enthält, um mit regulären Ausdrücken zu arbeiten.

Um zum Beispiel alle Zahlen aus einem String zu entfernen, könnten wir die `Regex.contains` Funktion verwenden und eine entsprechende Regular Expression erstellen:

``` Elm
let myString = "Elm ist 1 große 2 Sprache"
let regex = Regex.fromRegex "\\d+" |> Result.withDefault Regex.never
Regex.contains regex myString
-- gibt "Elm ist große Sprache" aus
```

In diesem Beispiel erstellen wir zuerst ein `Regex`-Objekt mit dem Muster `\\d+`, das jede Zahl im String identifiziert. Dann verwenden wir die `Regex.contains` Funktion, um zu überprüfen, ob der String eine Übereinstimmung mit dem Muster hat, und entfernen alle gefundenen Zahlen.

## Siehe auch
- [Offizielle Elm Dokumentation zu String.filter](https://package.elm-lang.org/packages/elm/core/latest/String#filter)
- [Offizielle Elm Dokumentation zur Regex-Bibliothek](https://package.elm-lang.org/packages/elm/regex/latest/)