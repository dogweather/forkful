---
title:                "Haskell: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Det kan finnas många anledningar till varför någon skulle vilja veta om en viss mapp eller katalog existerar. Det kanske är för att kontrollera om en fil ska sparas på rätt plats, eller för att se om en viktig resurs är tillgänglig innan man fortsätter med ett program. Oavsett orsaken är det en viktig del av programmering att kunna hantera sådana situationer. I Haskell är det enkelt att kontrollera om en mapp existerar eller inte.

## Hur man gör det

För att kontrollera om en mapp eller katalog existerar i Haskell kan du använda funktionen `doesDirectoryExist` från modulen `System.Directory`. Här är ett enkelt exempel som visar hur den kan användas:

```Haskell
import System.Directory

main = do
    exists <- doesDirectoryExist "minmapp"
    if exists
        then putStrLn "Mappen finns!"
        else putStrLn "Mappen finns inte."
```

I detta exempel kontrollerar vi om mappen "minmapp" existerar på vår dator. Om den gör det, skriver vi ut ett meddelande som bekräftar det. Om den inte existerar skriver vi istället ut ett annat meddelande. En enkel men användbar funktion!

## Djupdykning

Nu när vi vet hur man kontrollerar om en mapp eller katalog existerar i Haskell kan vi titta lite närmare på hur funktionen `doesDirectoryExist` fungerar. Den tar en sträng, som är sökvägen till mappen eller katalogen som ska kontrolleras, som argument och returnerar sedan en boolesk värde som indikerar om mappen existerar eller inte. Om sökvägen är relativ, så utgår den från den aktuella arbetsmappen. Om sökvägen är absolut, så används den direkt. Detta innebär också att sökvägen måste anges i rätt form beroende på det operativsystem som används.

Funktionen `doesDirectoryExist` är också rekursiv och kommer att söka igenom alla undermappar för att kontrollera om de också existerar. Om åtkomsträttigheter hindrar åtkomst till någon av undermapparna, så kommer funktionen att returnera `False` oavsett om mappen faktiskt existerar eller inte.

## Se även

- [Haskell.org - Working with Directories](https://www.haskell.org/hoogle/?hoogle=directory)
- [Haskell for Mac - File System Interaction](http://haskellformac.com/guides/basics/filesystem.html)
- [HaskellWiki - System.Directory](https://wiki.haskell.org/System.Directory)