---
title:                "Kontrollera om en mapp finns"
html_title:           "Haskell: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Kontroll av om en katalog finns är en process där en programmerare använder kod för att se om en viss katalog finns på datorn. Detta är ett viktigt steg i utvecklingen av program, eftersom det tillåter programmeraren att hantera fall där en viss katalog inte finns.

## Hur man gör:
Det enklaste sättet att kontrollera om en katalog finns är att använda funktionen ```doesDirectoryExist``` från ```System.Directory``` biblioteket i Haskell. Här är ett exempel på kod som använder den funktionen:

```Haskell
import System.Directory

main :: IO ()
main = do
  exists <- doesDirectoryExist "katalogen"
  if exists
    then putStrLn "Katalogen finns."
    else putStrLn "Katalogen finns inte."
```

Detta kodexempel kontrollerar om katalogen "katalogen" finns och skriver ut antingen "Katalogen finns." om den gör det, eller "Katalogen finns inte." om den inte gör det.

## Fördjupning:
Kontroll av om en katalog finns är en vanlig uppgift i många programmeringsspråk och bibliotek. I andra språk kan det dock krävas att man skriver mer komplicerad kod för att uppnå samma resultat. I Haskell kan man använda sig av ```doesDirectoryExist``` funktionen, men det finns också andra alternativ, som att använda ```getDirectoryContents``` för att hämta en lista av alla kataloger och sedan använda  ```elem``` funktionen för att se om en viss katalog finns i listan.

Det är också värt att notera att funktionen ```doesDirectoryExist``` inte bara returnerar en boolesk variabel, utan faktiskt returnerar en typ som heter ```IO Bool```. Detta beror på att funktionen är "inbakad" i IO-monaden, vilket tillåter Haskell att hantera I/O-operationer på ett säkert och kontrollerat sätt.

## Se även:
- [Hackage - System.Directory](https://hackage.haskell.org/package/directory)
- [Officiell Haskell-dokumentation - System.Directory](https://www.haskell.org/cabal/users-guide/package-descriptions.html#build-depends)
- [Övning - Kontrollera om en fil finns](https://guide.aelve.com/haskell/check-if-a-directory-exists-in-a-jiffy-121af690.html)