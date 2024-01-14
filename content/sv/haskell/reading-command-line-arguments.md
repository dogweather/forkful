---
title:                "Haskell: Läsning av kommandoradsargument"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Att läsa in kommandoradsargument är en vanlig och användbar funktion i många program. Genom att läsa in dessa argument kan du ge ditt program olika beteenden och parametrar beroende på hur det är kallat. Fortsätt läsa för att lära dig hur du kan implementera detta i ditt Haskell-program.

## Hur man gör

För att kunna läsa in kommandoradsargument måste du importera modulen `System.Environment` i ditt Haskell-program.

```Haskell
import System.Environment
```

Nästa steg är att använda funktionen `getArgs` från denna modul för att läsa in argumenten. Denna funktion returnerar en lista av strängar som representerar de olika argumenten som har skickats till ditt program. Här är ett enkelt exempel:

```Haskell
import System.Environment

main = do
  args <- getArgs
  putStrLn ("Första kommandoradsargumentet är: " ++ head args)
```

Om du kör detta program med argumentet `hello`, kommer du att få utskriften "Första kommandoradsargumentet är: hello". Du kan också använda funktionen `length` för att se hur många argument som har skickats till ditt program.

```Haskell
import System.Environment

main = do
  args <- getArgs
  putStrLn ("Antal kommandoradsargument: " ++ show (length args))
```

Om du till exempel kör detta program med argumenten `hello world`, kommer du att få utskriften "Antal kommandoradsargument: 2". Du kan också använda funktionen `map` för att utföra en operation på varje argument i listan.

```Haskell
import System.Environment

main = do
  args <- getArgs
  putStrLn ("Alla kommandoradsargument: " ++ show (map (++"!") args))
```

Om du kör detta program med argumenten `hello world`, kommer du att få utskriften "Alla kommandoradsargument: ["hello!", "world!"]".

## Djupdykning

När du läser in kommandoradsargument, är det viktigt att komma ihåg att de alltid läses in som strängar. Detta innebär att om du behöver använda dem som andra datatyper, till exempel som heltal eller listor, måste du konvertera dem först. För att konvertera en sträng till ett heltal, kan du använda funktionen `read`. Om du till exempel vill konvertera det första kommandoradsargumentet till ett heltal, kan du göra det på följande sätt:

```Haskell
import System.Environment

main = do
  args <- getArgs
  let num = read (head args) :: Int
  putStrLn ("Första argumentet som heltal: " ++ show num)
```

Om du kör detta program med argumentet `123`, kommer du att få utskriften "Första argumentet som heltal: 123". Kom också ihåg att du alltid behöver använda `::` för att specificera vilken typ du vill konvertera till.

## Se också

- [Haskell dokumentation för System.Environment modulen](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-Environment.html)
- [Haskell Forall Blogg: "Läsa in kommandoradsargument i Haskell"](https://www.haskellforall.com/2018/01/reading-command-line-arguments-in-haskell.html)
- [WikiBooks: "Kommandoradsargument i Haskell"](https://en.wikibooks.org/wiki/Haskell/Command_line_arguments)