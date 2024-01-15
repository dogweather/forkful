---
title:                "Eine Zeichenkette großschreiben"
html_title:           "Haskell: Eine Zeichenkette großschreiben"
simple_title:         "Eine Zeichenkette großschreiben"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man überhaupt einen String in Haskell großschreiben wollen? Ganz einfach: Es kann in vielen Situationen nützlich sein, bestimmte Wörter oder Sätze hervorzuheben oder mit der Konvention der Großschreibung in einer anderen Programmiersprache zu arbeiten.

## Wie man String in Haskell großschreibt

Um einen String in Haskell großzuschreiben, gibt es mehrere Möglichkeiten. Eine davon ist die `toUpper` Funktion, die einfach alle Buchstaben in einem String in Großbuchstaben umwandelt. Zum Beispiel:

```Haskell
toUpper "hello world" -- Ausgabe: "HELLO WORLD"
```

Du kannst auch mehrere Strings mit Hilfe von `map` großschreiben:

```Haskell
let strings = ["Haskell", "ist", "toll"]
map toUpper strings -- Ausgabe: ["HASKELL", "IST", "TOLL"]
```

## Tiefergehende Erklärung

Was passiert eigentlich hinter den Kulissen, wenn wir einen String in Haskell großschreiben? Für eine tiefergehende Erklärung schauen wir uns die Definition der `toUpper` Funktion an: 

```Haskell
toUpper :: Char -> Char
toUpper char = 
  if char >= 'a' && char <= 'z'
    then chr (ord char - ord 'a' + ord 'A')
    else char
```

Hier sehen wir, dass die `toUpper` Funktion zuerst prüft, ob der Buchstabe im Bereich von 'a' bis 'z' liegt. Wenn ja, wird er mit Hilfe der `chr` und `ord` Funktionen in den entsprechenden Großbuchstaben umgewandelt. Ansonsten wird der Buchstabe unverändert zurückgegeben.

## Siehe auch

- [Offizielle Haskell Dokumentation](https://www.haskell.org/documentation/)
- [Codebeispiele für Strings in Haskell](https://wiki.haskell.org/Strings/zh