---
date: 2024-01-20 17:56:02.681646-07:00
description: "Beim Lesen von Kommandozeilenargumenten nimmt ein Programm Parameter\
  \ von au\xDFen auf, um sein Verhalten zu steuern oder Daten zu verarbeiten. Programmierer\u2026"
lastmod: 2024-02-19 22:05:12.873890
model: gpt-4-1106-preview
summary: "Beim Lesen von Kommandozeilenargumenten nimmt ein Programm Parameter von\
  \ au\xDFen auf, um sein Verhalten zu steuern oder Daten zu verarbeiten. Programmierer\u2026"
title: Lesen von Kommandozeilenargumenten
---

{{< edit_this_page >}}

## Was & Warum?
Beim Lesen von Kommandozeilenargumenten nimmt ein Programm Parameter von außen auf, um sein Verhalten zu steuern oder Daten zu verarbeiten. Programmierer nutzen dies, um ihre Programme flexibler und interaktiver zu gestalten.

## How to:
```Haskell
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    print args
```
Ausführen im Terminal:
```bash
runghc myprogram.hs arg1 arg2 arg3
```
Ausgabe:
```Haskell
["arg1", "arg2", "arg3"]
```

## Deep Dive
Haskell, seit den späten 80ern entwickelt, bietet über das Modul `System.Environment` eine elegante Art, Kommandozeilenargumente zu lesen. Alternativ gibt es Bibliotheken, etwa `optparse-applicative`, für komplexere Argumente und schönere Hilfe-Texte. Die einfache `getArgs`-Funktion legt eine Liste von Strings an, wo jedes der Argumente eins zu eins übergeben wird. Wer mehr Kontrolle braucht, kann auf `getOpt` zurückgreifen, wo Parameter genauer spezifiziert werden können.

## See Also
- [Haskell `getArgs` documentation](https://hackage.haskell.org/package/base/docs/System-Environment.html#v:getArgs)
- [optparse-applicative auf Hackage](https://hackage.haskell.org/package/optparse-applicative)
- [getOpt Tutorial](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-Console-GetOpt.html)
