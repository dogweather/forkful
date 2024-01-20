---
title:                "Läsa en textfil"
html_title:           "Fish Shell: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att läsa en textfil i programmering innebär att extrahera data från en fil för att sedan manipulera den i koden. Vi gör det för att lagra och använda data på ett effektivt sätt.

## Hur gör man:

I Haskell kan vi läsa en textfil med hjälp av `readFile` funktionen. Här är ett exempel:

```Haskell
main = do  
    content <- readFile "test.txt"  
    putStrLn content  
```
I det här exemplet läses filen "test.txt" och skriver ut innehållet i terminalen.

## Fördjupa sig

`readFile` funktionen kom till Haskell under 1997, vilket visar på språkets starka fokus på I/O-interaktioner redan tidigt. Ett alternativ till `readFile` är att använda `hGetContents`, som ger mer flexibilitet men också kräver en större kodinsats.

För att förstå hur `readFile` fungerar internt, är det viktigt att notera att det i Haskell är en ”lazy” funktion. Det innebär att den bara läser delar av filen när data behövs, istället för att läsa hela filen direkt. Detta kan vara mycket effektivt för stora filer.

## Visa även

För mer information om I/O i Haskell, kolla in följande länkar:

- [Real World Haskell: Input and Output](http://book.realworldhaskell.org/read/io.html)
- [Learn You a Haskell for Great Good: Input and Output](http://learnyouahaskell.com/input-and-output)

Kodexempel för `hGetContents` kan hittas [här](https://www.haskell.org/tutorial/io.html).