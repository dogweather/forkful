---
date: 2024-01-20 17:51:06.798729-07:00
description: "String-Interpolation erm\xF6glicht es, Variablenwerte direkt in Strings\
  \ einzuf\xFCgen. Das macht den Code \xFCbersichtlicher und erleichtert die Erstellung\u2026"
lastmod: '2024-02-25T18:49:50.975735-07:00'
model: gpt-4-1106-preview
summary: "String-Interpolation erm\xF6glicht es, Variablenwerte direkt in Strings\
  \ einzuf\xFCgen. Das macht den Code \xFCbersichtlicher und erleichtert die Erstellung\u2026"
title: Zeichenketten interpolieren
---

{{< edit_this_page >}}

## Was & Warum?
String-Interpolation ermöglicht es, Variablenwerte direkt in Strings einzufügen. Das macht den Code übersichtlicher und erleichtert die Erstellung dynamischer Textausgaben.

## How to:
In Haskell gibt es keine eingebaute String-Interpolation wie in manch anderen Sprachen. Stattdessen nutzen wir die `printf`-Funktion aus dem `Text.Printf`-Modul oder die Template-Strings aus dem `interpolate`-Paket. Hier sind zwei Methoden:

Mit `printf`:

```haskell
import Text.Printf

main :: IO ()
main = do
    let name = "Welt"
    printf "Hallo, %s!\n" name
```

Ausgabe:
```
Hallo, Welt!
```

Mit Template-Strings:

Zuerst, fügen Sie das `interpolate`-Paket Ihrem Projekt hinzu, indem Sie die entsprechende Zeile in Ihrer `.cabal`-Datei oder `stack.yaml` aufnehmen. Jetzt verwenden Sie es:

```haskell
{-# LANGUAGE QuasiQuotes #-}
import Data.String.Interpolate (i)

main :: IO ()
main = do
    let name = "Welt"
    putStrLn [i|Hallo, #{name}!|]
```

Ausgabe:
```
Hallo, Welt!
```

## Deep Dive
Historisch gesehen hatte Haskell keine eingebaute String-Interpolation, hauptsächlich wegen des starken Fokus auf Typsicherheit und Funktionsreinheit. Frühe Haskeller benutzten `++` für die Verkettung von Strings oder `printf` für formatierte Ausgaben, ähnlich wie in C.

Das Paket `Text.Printf` bringt C-ähnliche `printf`-Funktionalität nach Haskell, während die `interpolate`-Bibliothek ein moderneres Gefühl mit Template-Strings bietet. Die Implementierung nutzt QuasiQuotes, eine Erweiterung, die es ermöglicht, Strings zu parsen und in Haskell-Ausdrücke umzuwandeln.

Alternativen sind Monoid-Operationen mit `(++)` oder `concat`, sowie `show` für die Umwandlung von Daten in Strings. Viele Haskeller verwenden auch Formatierungsbibliotheken wie `formatting`, die leistungsfähig sind, aber eine lernkurve haben.

## See Also
Hier sind einige Ressourcen, um mehr über String-Interpolation und verwandte Themen in Haskell zu erfahren:

- [Text.Printf Dokumentation](https://hackage.haskell.org/package/base-4.15.0.0/docs/Text-Printf.html)
- [`interpolate` Paket auf Hackage](https://hackage.haskell.org/package/interpolate)
- [`formatting` Paket auf Hackage](https://hackage.haskell.org/package/formatting)
- [HaskellWiki zu Strings](https://wiki.haskell.org/Strings)
- [Learn You a Haskell für großartige Güte!](http://learnyouahaskell.com/chapters)
