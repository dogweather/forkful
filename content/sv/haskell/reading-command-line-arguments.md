---
title:                "Läsa kommandoradsargument"
html_title:           "Bash: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa in kommandoradsargument innebär att ett program tar data direkt vid körning. Detta är användbart för att ändra programmets beteende utan att ändra koden.

## Så här gör du:

Här är ett exempel på hur du läser kommandoradsargument i Haskell:

```Haskell
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    print args
```
Om du sparar denna fil som `args.hs` och kör `runhaskell args.hs hello world`, får du:

```Haskell
["hello", "world"]
```
## Fördjupning

Kommandoradsargument introducerades för länge sedan i tidiga operativsystem och har sedan dess varit en viktig del av programmeringsmetodiken. Alternativ till kommandoradsargument inkluderar att läsa in en konfigurationsfil eller använda interaktiv inmatning.

Vad gäller implementationen använder `getArgs`-funktionen i Haskell IO-monaden för att läsa in argumenten. IO-monaden används i Haskell för att hantera sidoeffekter, vilket inkluderar kommandoradsinteraktioner.

## Se också

Ytterligare resurser för att lära dig mer om detta koncept:

1. Real World Haskell, kapitel 8 (https://book.realworldhaskell.org/read/io.html)
2. HaskellWiki, Command Line Arguments (https://wiki.haskell.org/Command_line_argument_parsing)
3. Learn You a Haskell, Input and Output (http://learnyouahaskell.com/input-and-output)