---
title:    "Haskell: Kontrollera om en mapp finns"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en mapp existerar är en viktig del av programmering. Det kan hjälpa till att säkerställa att programmet fungerar korrekt och förhindra eventuella felmeddelanden från att visas.

## Hur man gör det

För att kontrollera om en mapp existerar i Haskell kan du använda funktionen `doesDirectoryExist` från modulen`System.Directory`. Nedan finns ett exempel på hur denna funktion kan användas:

```Haskell
import System.Directory

existerarMapp :: FilePath -> IO Bool
existerarMapp mapp = doesDirectoryExist mapp

main = do
    mappExisterar <- existerarMapp "testmapp"
    if mappExisterar
        then putStrLn "Mappen existerar."
        else putStrLn "Mappen existerar inte."
```

I detta exempel tar funktionen `doesDirectoryExist` en `FilePath` som argument, vilket är sökvägen till den mapp som ska kontrolleras. Om mappen existerar så returnerar den `True`, annars returnerar den `False`.

## Djupdykning

Det finns flera olika sätt att kontrollera om en mapp existerar i Haskell, men funktionen `doesDirectoryExist` är vanligtvis den enklaste och mest pålitliga metoden. Detta är eftersom den använder operativsystemets egna funktioner för att utföra kontrollen, vilket gör den plattformsoberoende.

För att något annat än `True` eller `False` ska returneras från `doesDirectoryExist` måste du använda funktionen `catch` från modulen `Control.Exception`. Med denna funktion kan du fånga eventuella felmeddelanden som kan uppstå, vilket kan vara användbart om du till exempel vill hantera användarfel eller andra specialfall.

## Se även

* [Hackage - System.Directory](https://hackage.haskell.org/package/directory)
* [Haskell.org - System.Directory](https://www.haskell.org/hoogle/?hoogle=System.Directory)
* [Hindi programming blog article](https://www.hindi.com/writing-blog-posts)