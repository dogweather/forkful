---
title:                "Skapa en tillfällig fil"
html_title:           "Haskell: Skapa en tillfällig fil"
simple_title:         "Skapa en tillfällig fil"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skapandet av en tillfällig fil är en vanlig uppgift inom programmering som innebär att temporärt skapa en fil på datorn för att använda i ett program. Det görs oftast för att lagra data som endast är nödvändig under en kort period.

## Så här:
Ett enkelt sätt att skapa en tillfällig fil i Haskell är att använda funktionen `withSystemTempFile`, som tar emot en action som argument och skapar en tillfällig fil som sedan tas bort automatiskt efter att actionen är utförd. Se ett exempel nedan:
```Haskell
withSystemTempFile "example.txt" $ \path handle -> do
    hPutStrLn handle "Detta är en tillfällig fil!"
    putStrLn ("Filen har skapats på sökvägen: " ++ path)
```

Output:
```
Filen har skapats på sökvägen: /tmp/example.txt
```
Det finns också möjlighet att manuellt skapa en tillfällig fil med hjälp av funktionen `openTempFile`, som tar emot en sökväg och filnamn som argument och returnerar en tupel med en handle till filen och sökvägen till den skapade filen.

## Djupdykning:
Skapandet av tillfälliga filer är en viktig del av många programmeringsspråk och används ofta för att temporärt lagra data som endast behövs under en kort period. I tidigare versioner av Haskell behövde man använda externa bibliotek för att kunna skapa tillfälliga filer, men med introduktionen av `withSystemTempFile` och `openTempFile` i Haskell 98 har denna process blivit enklare.

Som alternativ till att skapa tillfälliga filer kan man även använda sig av andra metoder för att temporärt lagra data, såsom att använda minnesbufferten eller att skapa en temporär databas. Det är viktigt att välja rätt metod beroende på programmets behov och krav.

## Se även:
- [Haskell Documentation: System.IO.Temp](https://hackage.haskell.org/package/base/docs/System-IO-Temp.html)
- [Learn You a Haskell: File I/O](http://learnyouahaskell.com/input-and-output#files-and-streams)