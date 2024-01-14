---
title:    "Haskell: Skriva en textfil"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Varför

Att kunna skriva textfiler är en viktig färdighet inom Haskell-programmering. Genom att behärska denna färdighet kan du skapa program som kan hantera textbaserad input och output. Detta är användbart för att skapa användarvänliga gränssnitt och för att läsa och skriva till filer.

## Hur man gör

För att skriva en textfil i Haskell måste du först importera modulen `System.IO`. Sedan kan du använda funktionen `writeFile` för att skapa och skriva till en fil. En enkel kodexempel skulle se ut som följande:

```Haskell
import System.IO

main = do
    let text = "Detta är en textfil som kommer att skrivas på skrivbordet."
    writeFile "skrivbord/textfil.txt" text
```

När du sedan kör detta program kommer det att skapa en fil med namnet "textfil.txt" på din skrivbordsmapp. Om filen redan finns kommer den att ersättas med den nya texten som du skriver in i `writeFile` funktionen.

Du kan också använda funktionen `appendFile` för att lägga till text i en befintlig fil istället för att ersätta allt innehåll. 

En annan viktig sak att notera när man skriver till textfiler är att det är viktigt att använda rätt kodning för filen. I Haskell är standardkodningen UTF-8, men du kan använda funktionen `hSetEncoding` för att ändra det om det behövs.

## Djupdykning

När du skriver till en textfil i Haskell kommer det att resultera i att en IO (Input/Output) "action" skapas. Detta innebär att skrivningen till filen inte utförs direkt, utan den samlas in i en kö och utförs senare när du kör programmet i en särskild kontext vid namn "main". 

Detta är en kraftfull funktion i Haskell eftersom det gör det möjligt att separera dina funktionella program från de "imperativa" instruktionerna för input och output. Det minimerar också risken för buggar och fel.

Det finns också flera andra användbara funktioner som du kan använda när du skriver till textfiler, såsom `writeChar` för att skriva en enskild bokstav och `writeStrLn` för att skriva en hel rad med text.

## Se även

- [Haskell Dokumentation - Textfiler](https://www.haskell.org/documentation)
- [Haskell Programmering - Skrivning av Textfiler](https://wiki.haskell.org/I/O_for_Imperative_Programmers/TextFiles)