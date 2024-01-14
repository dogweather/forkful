---
title:    "Elm: Berechnen der Länge eines Strings"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Es ist oft notwendig, die Länge eines Strings in einer Programmiersprache zu bestimmen. In Elm können wir dies auf einfache Weise tun, um die Anzahl der Zeichen in einem Text zu bestimmen.

## Wie man es macht

Um die Länge eines Strings in Elm zu finden, können wir die build-in Funktion `String.length` verwenden. Schauen wir uns ein Beispiel an:

```Elm
main = 
let
    text = "Hallo Welt"
    length = String.length text
in
    length

```

Die Ausgabe dieses Codes ist 11, da die Zeichenkette "Hallo Welt" 11 Zeichen hat. Wir definieren zuerst eine Variable `text`, die unseren String enthält. Dann verwenden wir die Funktion `String.length`, um die Länge des Strings zu bestimmen und weisen das Ergebnis der Variablen `length` zu. Schließlich geben wir mithilfe von `in` die Länge unseres Textes aus.

## Tiefergehender Einblick

Die `String.length` Funktion gibt die Anzahl der UTF-8 Codierungseinheiten zurück, nicht die tatsächliche Anzahl der Zeichen im String. Dies bedeutet, dass Zeichen aus Nicht-Lateinischen Alphabeten oder Emojis, die aus mehreren Codierungseinheiten bestehen, als mehrere Zeichen gezählt werden. Um die tatsächliche Anzahl der Zeichen in einem String zu erhalten, gibt es einige Techniken, die wir anwenden können, wie zum Beispiel das Aufteilen des Strings in ein Array und die Verwendung der Länge dieses Arrays.

## Siehe auch

- Offizielle Dokumentation zur Funktion `String.length`: https://package.elm-lang.org/packages/elm/core/latest/String#length
- Eine Erklärung von UTF-8 und dessen Auswirkungen auf die String-Länge: https://www.elm-tutorial.org/de/03-strings/02-utf8.html
- Ein Beispiel, wie man die tatsächliche Anzahl von Zeichen in einem String bestimmt: https://discourse.elm-lang.org/t/string-length-in-elm-unicode-and-utf8/3869