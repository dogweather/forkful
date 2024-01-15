---
title:                "Löschen von Zeichen, die einem Muster entsprechen"
html_title:           "Elm: Löschen von Zeichen, die einem Muster entsprechen"
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Manchmal müssen wir in unserer Programmierung Texte bearbeiten und dabei bestimmte Zeichen entfernen. Das kann zum Beispiel nützlich sein, wenn wir bestimmte Muster oder Formatierungen aus unseren Daten entfernen wollen. In Elm können wir dies auf einfache und effektive Weise tun. In diesem Artikel lernen wir, wie man Zeichen entfernt, die einem bestimmten Muster entsprechen.

## Wie geht’s

In Elm gibt es die Funktion `String.filter`, mit der wir Zeichen aus einer Zeichenkette entfernen können, die einem bestimmten Muster entsprechen. Hier ist ein Beispiel, wie wir alle Zahlen aus einer Zeichenkette entfernen können:

```Elm
String.filter (\char -> not (Char.isDigit char)) "5a7m1n9"
```

Das Ergebnis dieser Funktion ist die Zeichenkette `"amn"`. Jetzt lassen Sie uns dieses Beispiel Schritt für Schritt analysieren:

1. Wir geben der Funktion `String.filter` zwei Argumente: eine Funktion und eine Zeichenkette.
2. Die Funktion, die wir übergeben, hat das Argument `char` vom Typ `Char` und gibt einen booleschen Wert zurück.
3. Hier verwenden wir die Funktion `not` und die Funktion `Char.isDigit`, um zu überprüfen, ob das Zeichen `char` keine Zahl ist.
4. Die Zeichenkette `"5a7m1n9"` wird als zweites Argument übergeben.
5. Die Funktion `String.filter` durchläuft jedes Zeichen in der Zeichenkette und gibt eine neue Zeichenkette zurück, in der alle Zeichen, die nicht dem Muster entsprechen, entfernt wurden.

Das ist alles, was wir tun müssen, um Zeichen zu entfernen, die einem bestimmten Muster entsprechen. Es gibt viele weitere nützliche Funktionen in Elm, die uns beim Bearbeiten von Texten helfen können. Im Folgenden sind einige nützliche Links aufgelistet, die Sie erkunden können.

## Tiefentauchen

Wenn Sie sich für die technischen Details hinter `String.filter` interessieren, ist hier eine kleine Übersicht:

Die Funktion `String.filter` nimmt eine Funktion und eine Zeichenkette entgegen und gibt eine neue Zeichenkette zurück. Diese neue Zeichenkette wird durch die alte Zeichenkette iteriert und jedes Zeichen wird in die übergebene Funktion eingegeben. Wenn die Funktion `True` zurückgibt, wird das Zeichen in die neue Zeichenkette aufgenommen, ansonsten wird es ignoriert.

Warum ist das nützlich? Nun, in unserem Beispiel haben wir die Funktion `not` verwendet, um zu überprüfen, ob das Zeichen nicht dem Muster entspricht, aber Sie können jede Funktion übergeben, die einen booleschen Wert zurückgibt. Dadurch haben Sie volle Kontrolle darüber, welche Zeichen in Ihrer neuen Zeichenkette enthalten sein sollen.

## Siehe auch

- [Weitere Informationen über `String.filter` in der offiziellen Dokumentation](https://package.elm-lang.org/packages/elm/core/latest/String#filter)
- [Die Funktion `removeNumber` in dieser Beispielanwendung zeigt verschiedene Möglichkeiten, Zeichen zu entfernen.](https://ellie-app.com/7sK56xKNnD4a1)