---
title:                "Kontrollera om en katalog finns"
html_title:           "Haskell: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför
Vad är poängen med att kolla om en mapp existerar? Svaret är enkelt: för att undvika att köra kod som förväntar sig att mappen finns, och för att hantera eventuella felaktiga sökvägar innan de leder till problem.

## Hur man gör
Det finns flera sätt att kontrollera om en mapp existerar i Haskell. Ett sätt är att använda funktionen `doesDirectoryExist` från modulen `System.Directory` som returnerar en `Bool` beroende på om mappen existerar eller inte.

```Haskell
import System.Directory

checkIfDirectoryExists :: FilePath -> IO Bool
checkIfDirectoryExists path = doesDirectoryExist path

main = do
    let directoryPath = "path/to/directory"
    exists <- checkIfDirectoryExists directoryPath
    if exists
        then putStrLn "Mappen finns!"
        else putStrLn "Mappen finns inte!"
```

Koden ovan importeras först modulen `System.Directory` som innehåller funktionen `doesDirectoryExist`. Sedan definierar vi en funktion `checkIfDirectoryExists` som tar en sökväg (`FilePath`) som argument och använder `doesDirectoryExist` för att kontrollera om mappen existerar. I huvudprogrammet så bestämmer vi en sökväg och använder sedan funktionen `checkIfDirectoryExists` för att få en `Bool` som vi sedan använder för att skriva ut ett meddelande beroende på resultatet.

## Djupdykning
Funktionen `doesDirectoryExist` använder sig av systemanrop för att kolla om en mapp existerar. Detta innebär att den är beroende av vilket operativsystem som körs och hur dess filsystem är strukturerat.

Det finns också andra sätt att kontrollera om en mapp existerar i Haskell, till exempel genom att använda funktionen `getPathStatus` från modulen `System.Posix.Files` eller funktionen `findFile` från modulen `System.FilePath.Find`.

## See Also
Kolla gärna in dessa länkar för mer information om fil- och mapphantering i Haskell:

- [Hackage-Dokumentation för System.Directory](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Hackage-Dokumentation för System.Posix.Files](https://hackage.haskell.org/package/unix/docs/System-Posix-Files.html)
- [Hackage-Dokumentation för System.FilePath.Find](https://hackage.haskell.org/package/filepath-find/docs/System-FilePath-Find.html)