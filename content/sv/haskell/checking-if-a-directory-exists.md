---
title:                "Haskell: Kontrollera om en mapp finns"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför
Att kontrollera om en mapp existerar är en viktig del av att skapa stabila och pålitliga program. Genom att ha ett sätt att hantera fall där en mapp kanske inte finns, kan du undvika kraschar och felmeddelanden.

## Hur man gör
Det finns flera sätt att kontrollera om en mapp existerar i Haskell. Ett enkelt sätt är att använda "doesDirectoryExist" funktionen från "System.Directory" modulen. Denna funktion tar en sökväg som argument och returnerar en Bool, True om mappen existerar och False annars.

```Haskell
import System.Directory

-- Kontrollera om mappen "test" existerar
doesDirectoryExist "test"
-- Output: False
```

En annan metod är att använda "listDirectory" funktionen. Denna funktion returnerar en lista av alla filer och mappar i en given sökväg. Genom att använda "filter" funktionen kan du sedan kontrollera om en specifik mapp är i listan.

```Haskell
import System.Directory

-- Hämta en lista av alla filer och mappar i "test" mappen
listDirectory "test"
-- Output: ["file1.txt", "file2.txt", "subdir"]

-- Kontrollera om "subdir" mappen finns i listan
elem "subdir" $ listDirectory "test"
-- Output: True
```

## Djupdykning
En annan viktig aspekt att tänka på när du kontrollerar om en mapp existerar är eventuella rättighetsproblem. Om ditt program inte har rättigheter att läsa en viss mapp kan "doesDirectoryExist" funktionen ge ett felaktigt resultat. Det är också viktigt att notera att både "doesDirectoryExist" och "listDirectory" funktionen använder sig av "IO" monaden, vilket innebär att du behöver använda en "do" notation för att använda dessa funktioner.

## Se också
- [System.Directory dokumentation](https://hackage.haskell.org/package/base-4.15.1.0/docs/System-Directory.html)
- [Guide för hantering av filer och mappar i Haskell](https://www.codementor.io/@meatflavour/an-introduction-to-working-with-files-in-haskell-31mwx6rh6l)