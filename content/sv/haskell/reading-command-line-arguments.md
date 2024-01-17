---
title:                "Läsning av kommandoradsargument"
html_title:           "Haskell: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Vad & Varför?
Läsning av kommandoradsargument är en vanlig uppgift för Haskell-programmerare. Det är det sätt på vilket ett program kan ta emot input från användaren när det körs från en terminal. Detta gör det möjligt för användaren att anpassa programmet på ett dynamiskt sätt och utöka funktionaliteten.

# Hur?
Det finns ett inbyggt Haskell-bibliotek som heter "System.Environment", som ger funktioner för att läsa och hantera kommandoradsargument. Här är ett exempel på hur man kan använda det:

```Haskell
import System.Environment

main = do
  -- hämta en lista med alla argument som matats in
  args <- getArgs
  
  -- skriv ut det första argumentet
  putStrLn ("Det första argumentet är: " ++ args !! 0)
  
  -- skriv ut alla argument som har matats in
  putStrLn "Alla argument:"
  mapM_ putStrLn args
  
  -- hämta det andra argumentet och omvandla det till en Integer
  let num = read (args !! 1) :: Integer
  
  -- addera numret med sig självt och skriv ut resultatet
  putStrLn ("Det andra argumentet dubbel är: " ++ show (num + num))
```

Om vi kör detta program med följande kommandoradsargument:

```bash
runhaskell myprogram.hs hello 5
```

Kommer utskriften att bli:

```bash
Det första argumentet är: hello
Alla argument:
hello
5
Det andra argumentet dubbel är: 10
```

# Deep Dive
Kommandoradsargument läses in i form av en lista av strängar (strings) i Haskell. Detta är annorlunda jämfört med andra språk, såsom Java eller C++, där argumenten läses in som arrayer eller vectore. Detta gör processen lite annorlunda när det gäller att hantera eller manipulera argumenten.

Ett alternativ till att använda "System.Environment" är att använda en tredjepartsbibliotek som "OptParse-Applicative", vilket ger mer avancerade funktioner för att läsa och hantera kommandoradsargument.

Implementeringen av "System.Environment" är baserad på System.IO, vilket innebär att det är beroende av verktygshantering och inte direktaccess till terminalen. Det är viktigt att ha i åtanke när du arbetar med kommandoradsargument i Haskell.

# Se även
- https://wiki.haskell.org/Command_line_arguments
- https://hackage.haskell.org/package/base-4.15.0.0/docs/System-Environment.html
- https://github.com/pcapriotti/optparse-applicative