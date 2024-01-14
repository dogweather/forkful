---
title:    "Elm: Die Länge eines Strings finden"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Warum

Bevor wir uns mit dem Thema befassen, warum sollte man überhaupt die Länge einer Zeichenfolge berechnen? Ganz einfach: Es ist eine grundlegende Operation, die in vielen Programmiersprachen verwendet wird und daher auch auf Elm nicht verzichten sollte. Indem man die Länge einer Zeichenfolge berechnet, kann man zum Beispiel sicherstellen, dass eine Benutzereingabe nicht zu lang ist oder dass ein Text in ein bestimmtes Textfeld passt.

## Wie geht man vor?

Zunächst müssen wir die Funktion `String.length` importieren, welche uns die Länge einer Zeichenfolge zurückgibt. Dann können wir diese Funktion in unserem Code mit der gewünschten Zeichenfolge aufrufen. Hier ist ein Beispiel:

```Elm
import String

lengthOfString = String.length "Hallo Welt"
```

Dieser Code wird die Länge der Zeichenfolge `Hallo Welt` (11 Zeichen) berechnen und in der Variable `lengthOfString` speichern. Wir können auch Benutzereingaben verwenden, um die Länge einer dynamisch eingegebenen Zeichenfolge zu berechnen:

```Elm
import String

lengthOfInput = input |> String.length
```

Diese Funktion funktioniert auch mit Sonderzeichen und Umlauten.

## Tiefere Einblicke

Hinter den Kulissen verwendet Elm die Unicode-Zeiteinheit, um die Länge einer Zeichenfolge zu berechnen. Eine Zeichenfolge kann aus mehreren Unicode-Zeichen bestehen, daher ist der Wert, den `String.length` zurückgibt, die Anzahl der Unicode-Zeichen und nicht die Anzahl der tatsächlichen Buchstaben oder Zeichen.

Ein interessanter Aspekt von Elm ist, dass es keine integrierte Funktion gibt, um die Anzahl der tatsächlichen Buchstaben oder Zeichen zu berechnen. Allerdings gibt es viele Community-Pakete, die diese Funktionalität bereitstellen, wie zum Beispiel [elm-community/string-extra](https://package.elm-lang.org/packages/elm-community/string-extra/latest/String-Extra), das die Funktion `String.Extra.length` zur Verfügung stellt, um die Anzahl der Buchstaben zu zählen.

## Siehe auch

- [Elm-Dokumentation zu String.length](https://package.elm-lang.org/packages/elm/core/latest/String#length)
- [elm-community/string-extra Paket](https://package.elm-lang.org/packages/elm-community/string-extra/latest/String-Extra)