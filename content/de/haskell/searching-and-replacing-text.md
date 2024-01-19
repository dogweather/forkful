---
title:                "Suchen und Ersetzen von Text"
html_title:           "C#: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Suchen und Ersetzen von Text in Haskell

## Was & Warum?

Textsuche und -ersetzung sind Vorgänge, bei denen spezifische Zeichenfolgen in einem Text gefunden (gesucht) und durch andere Zeichenfolgen ersetzt werden. Programmierer tun dies, um automatische Textmodifikationen durchzuführen, Daten zu bereinigen oder zu analysieren.

## So geht's

Einfacher Textaustausch lässt sich mit der eingebauten `Data.List.Utils`-Bibliothek realisieren.

```Haskell
import Data.List.Utils
main = do
 let str = "Hallo, Haskell!"
 putStrLn $ replace "Haskell" "Welt" str
```

Ausgabe:

```Haskell
Hallo, Welt!
```

Für komplexe Fälle, etwa bei regulären Ausdrücken und mehrfachen Austausch, empfehlen wir die Verwendung der `Text.Regex`-Bibliothek.

```Haskell
import Text.Regex (subRegex, mkRegex)
main = do
 let regex = mkRegex "a[0-9]+"
 putStrLn $ subRegex regex "a123 b456 a789" "x"
```

Ausgabe:

```Haskell
x b456 x
```

## Tief tauchen

Die Technik des Suchens und Ersetzens ist ein Standbein des Text-Editierens und -Verarbeitens, seit dessen Anfängen in den 1940er Jahren. 

Mit Alternativen wie `Data.Text` lässt sich ebenso Text suchen und ersetzen, und es ist oft sinnvoll, die passende Bibliothek für Ihre spezifischen Bedürfnisse auszuwählen.

Die Implementierung von Such- und Ersetzvorgängen in Haskell beruht auf rein funktionaler Programmierung. Im Gegensatz zu imperativen Sprachen werden Daten hier nicht verändert, sondern es werden neue Daten erzeugt.

## Siehe auch

- [Haskell Tutorial](https://www.haskell.org/tutorial/)
- [Data.List.Utils Dokumentation](http://hackage.haskell.org/package/MissingH-1.4.0.1/docs/Data-List-Utils.html)
- [Text.Regex Dokumentation](http://hackage.haskell.org/package/regex-compat-0.95.2.0/docs/Text-Regex.html)