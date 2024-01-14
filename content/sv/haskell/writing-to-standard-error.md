---
title:    "Haskell: Att skriva till standard error"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför
Att skriva till standard error är en viktig del av att skriva Haskell-program. Det låter dig kommunicera med användaren på ett effektivt sätt och felsöka ditt program.

## Så här gör du
För att skriva till standard error i Haskell, behöver du använda funktionen `hPutStrLn` från modulen `System.IO`. Det här är en standardfunktion som tar en sträng som argument och skriver ut den till standard error.

```Haskell
import System.IO

main :: IO ()
main = do
    hPutStrLn stderr "Hej, världen!"
```

Output:
```
Hej, världen!
```

Du kan också använda `putStrLn`-funktionen för att skriva till standard output, men det rekommenderas att använda `hPutStrLn` för att se till att meddelandet skrivs ut på rätt ställe.

## Djupdykning
Standard error är en viktig del av felsökning i Haskell-program eftersom det låter dig skriva ut felmeddelanden och annan information till konsolen. Om du till exempel fångar ett fel med `catch`, kan du använda standard error för att skriva ut ett mer detaljerat felmeddelande.

```Haskell
import System.IO
import Control.Exception

main :: IO ()
main = do
    catch (do
        putStrLn "Skriv in ett heltal: "
        input <- getLine
        let num = read input :: Int
        putStrLn ("Ditt tal är: " ++ show num)
    ) (\e -> do 
        hPutStrLn stderr ("Ogiltigt inmatat värde: " ++ show (e :: SomeException))
    )
```

Om användaren matar in ett ogiltigt värde (t.ex. en bokstav istället för ett heltal) kommer det att skrivas ut ett mer tydligt felmeddelande på standard error.

## Se även
- [Haskell IO-dokumentation](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
- [En introduktion till felhantering i Haskell](https://dev.to/mattjbray/introduction-to-error-handling-in-haskell-3dmd)