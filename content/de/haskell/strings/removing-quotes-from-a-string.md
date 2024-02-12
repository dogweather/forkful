---
title:                "Anführungszeichen aus einem String entfernen"
aliases:
- /de/haskell/removing-quotes-from-a-string.md
date:                  2024-01-26T03:39:47.934905-07:00
model:                 gpt-4-0125-preview
simple_title:         "Anführungszeichen aus einem String entfernen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Anführungszeichen aus einem String zu entfernen bedeutet, jegliche Anführungszeichen—einfache (' ') oder doppelte (" ")—, die Teil der Stringdaten sind, herauszufiltern. Programmierer machen dies, um Eingaben zu bereinigen, Texte zur Verarbeitung vorzubereiten oder unnötige Zeichen loszuwerden, die die Datenverarbeitung und -operationen stören könnten.

## Wie zu:
In Haskell können wir eine Funktion erstellen, die alle Anführungszeichen aus einem gegebenen String entfernt. Es ist, als würde man den Anführungszeichen sagen, sie sollen verschwinden, und sich vergewissern, dass sie den Wink verstehen.

```Haskell
import Data.List (intercalate)
import Data.Char (isPunctuation)

removeQuotes :: String -> String
removeQuotes = filter (\c -> c /= '"' && c /= '\'')

main :: IO ()
main = do
    let stringWithQuotes = "Haskell sagte, \"Lass uns einige Funktionen lernen!\""
    putStrLn $ removeQuotes stringWithQuotes
```

Beispielausgabe:

```
Haskell sagte, Lass uns einige Funktionen lernen!
```

## Tiefer eintauchen
Es war einmal, bevor Strings in der Programmierung so allgegenwärtig waren wie Katzenvideos im Internet, da war der Umgang mit Text eine heikle Angelegenheit. Aber als die Programmiersprachen sich weiterentwickelten, wurden Strings ein wesentlicher Teil des Codierens. Dennoch blieben Anführungszeichen ein zweischneidiges Schwert—essenziell für die Definition von Strings, aber ein Ärgernis, wenn sie als tatsächliche Daten enthalten waren.

Alternativen? Anstatt alle Anführungszeichen wie Fliegen wegzuschlagen, können Sie selektiv sein. Vielleicht möchten Sie nur die äußersten Anführungszeichen entfernen (ein klassischer Trim) oder mit escapten Anführungszeichen innerhalb eines Strings umgehen.

Implementierungsweise verwendet die Funktion `removeQuotes` oben eine Lambda-Funktion, um jedes Zeichen (`c`) zu überprüfen, ob es ein lästiges Anführungszeichen ist, und filtert sie entsprechend heraus. Dies ist ein unkomplizierter Ansatz, aber für größere Texte oder komplexere Regeln möchten Sie vielleicht Parser-Bibliotheken wie `Parsec` betrachten, die Ihnen mehr Finesse und Kraft in der Textverarbeitung geben können.

## Siehe auch:
- Für Regex-Liebhaber: [Text.Regex.Posix](https://hackage.haskell.org/package/regex-posix)
- Eine sanfte Einführung in Haskell-Strings: [Learn You a Haskell for Great Good! - Starting Out](http://learnyouahaskell.com/starting-out#strings)
