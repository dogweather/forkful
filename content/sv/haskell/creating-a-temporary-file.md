---
title:    "Haskell: Skapa en tillfällig fil"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Varför
Att skapa en tillfällig fil kan vara användbart i många situationer, till exempel när du behöver spara data temporärt eller när du behöver skriva mellanresultat i en beräkningsprocess.

## Hur man gör
För att skapa en temporär fil i Haskell använder du funktionen `withSystemTempFile`, från `System.IO.Temp` biblioteket. Här är ett kodexempel:

```Haskell
import System.IO.Temp
import System.IO

main :: IO ()
main = do
  -- Skapar en temporär fil med namnet "tempfile.txt" i det nuvarande systemets temporära katalog.
  withSystemTempFile "tempfile.txt" $ \filePath handle -> do
    -- Skriver data till den temporära filen.
    hPutStrLn handle "Hej, detta är en temporär fil!"
    -- Stänger filhandeln.
    hClose handle
    
    -- Läser innehållet av den temporära filen.
    fileContent <- readFile filePath
    -- Skriver ut innehållet.
    putStrLn fileContent
```

Output:
```
Hej, detta är en temporär fil!
```

## Djupdykning
Genom att använda funktionen `withSystemTempFile`, behöver du inte oroa dig för att skapa eller ta bort själva den temporära filen. Funktionen tar hand om allt det här åt dig, inklusive hanteringen av fel och borttagning av filen när det inte längre behövs.

Förutom `withSystemTempFile`, finns det också en motsvarande funktion `withTempFile`, som låter dig specificera en egen katalog för den temporära filen.

Du kan också skapa och hantera temporära kataloger med hjälp av funktionerna `withSystemTempDirectory` och `withTempDirectory`.

## Se också
- [Haskell dokumentation för hantering av temporära filer](https://hackage.haskell.org/package/temporary/docs/System-IO-Temp.html)
- [Enkel guide till Haskell](https://www.codingame.com/playgrounds/21123/enkel-guide-till-haskell)
- [Haskell för nybörjare](https://wiki.haskell.org/Haskell_for_beginners)