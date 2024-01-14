---
title:                "Gleam: Unterstrings extrahieren"
simple_title:         "Unterstrings extrahieren"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

# Warum

Das Extrahieren von Teilzeichenfolgen ist eine häufige Aufgabe in der Programmierung. Es kann hilfreich sein, wenn Sie bestimmte Zeichen oder Wörter aus einem längeren Text extrahieren möchten. Mit Gleam können Sie diese Aufgabe schnell und einfach erledigen.

# Wie man es macht

Um Teilzeichenfolgen mit Gleam zu extrahieren, verwenden Sie die Funktion `String.substring()`. Hier ist ein Beispielcode, der die ersten 5 Zeichen eines Strings extrahiert:

```Gleam
let string = "Hallo, Welt!"
let substring = String.substring(string, 0, 5) // substring = "Hallo"
```

Sie können auch die Länge des extrahierten Substrings angeben, wie in diesem Beispiel, der die ersten 10 Zeichen extrahiert:

```Gleam
let string = "Gleam ist fantastisch!"
let substring = String.substring(string, 0, 10) // substring = "Gleam ist "
```

# Tiefer Einblick

Es ist wichtig zu beachten, dass der Index des ersten Zeichens in Gleam bei 0 beginnt. Dies bedeutet, dass das Zeichen an der Position 0 im Beispiel oben das "H" ist. Wenn Sie beispielsweise die ersten 3 Zeichen von "Hallo" extrahieren möchten, müssten Sie den Index von 0 bis 2 angeben.

Es gibt auch eine `String.substr() `Funktion in Gleam, die ähnlich wie `String.substring()` funktioniert, außer dass Sie nur den Anfangsindex angeben müssen und der Substring bis zum Ende des Strings extrahiert wird.

# Siehe auch

Hier sind einige nützliche Links, um mehr über das Extrahieren von Teilzeichenfolgen in Gleam zu erfahren:

- Offizielle Gleam-Dokumentation: https://gleam.run/documentation
- Gleam-Tutorial zum Arbeiten mit Zeichenfolgen: https://gleam.run/tutorials/strings
- Gleam-Quellcode auf GitHub: https://github.com/gleam-lang/gleam