---
title:                "Javascript: Läsa en textfil"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Varför du bör läsa en textfil

Att läsa en textfil kan vara en viktig del av att bli en bättre Javascript-programmerare. Att förstå hur man hanterar och bearbetar textfiler kan öppna dörrar till nya projekt och utvecklingsmöjligheter.

Förutom att det är en viktig färdighet att ha, kan läsning av textfiler också vara användbart i många olika situationer. Till exempel kan du hämta data från en annan applikation eller skapa en enkel databas med information som behöver sparas utanför din kod. Oavsett vilket är det en förmåga som alla Javascript-utvecklare bör ha.

# Hur man läser en textfil

För att läsa en textfil i Javascript behöver du först och främst en fil att läsa ifrån. Du kan skapa en textfil manuellt eller använda en befintlig fil. Sedan använder du inbyggda funktioner i Javascript för att öppna och läsa filen.

Här är ett enkelt exempel som visar hur man kan läsa en textfil och skriva ut innehållet till konsolen:

```Javascript
var fs = require('fs'); // importerar "fs" biblioteket för att läsa filer

fs.readFile('textfil.txt', 'utf8', function(err, data){ // öppnar filen och anger teckenkodning
    if(err){
        console.log(err); // om det finns ett fel, skriver ut det till konsolen
    } else{
        console.log(data); // om inga fel uppstår, skriver ut filens innehåll till konsolen
    }
});
```

I det här exemplet används funktionen `fs.readFile` för att öppna och läsa filen `textfil.txt`. Den andra parametern är teckenkodningen, som i de flesta fall är `utf8`. Om filen läses korrekt, kommer innehållet att skrivas ut till konsolen.

Det finns också andra sätt att läsa en textfil, som att använda funktionen `fs.readFileSync` för att läsa filen synkront eller använda `stream` för att läsa filen bit för bit. Beroende på dina behov och det specifika projektet, kan det finnas olika metoder som fungerar bäst för dig.

# Deep Dive - Djupdykning

Att läsa en textfil kan verka enkelt, men det finns faktiskt mer som händer bakom kulisserna. När du öppnar en fil i Javascript, skapar du en `File Descriptor`. Detta objekt innehåller information om filen och används för att läsa eller skriva till filen.

När du använder funktionerna `fs.readFile` eller `fs.readFileSync`, händer flera saker bakom kulisserna. Det första som händer är att filen öppnas och din data läses in i ett `Buffer` objekt. Därefter översätts buffern till den valda teckenkodningen (t.ex. `utf8`) och sedan returneras den till din kod.

Att förstå denna process kan hjälpa dig att felsöka problem som kan uppstå när du läser en fil, som felaktiga teckenkodningar eller felaktigt formatterade filer.

# Se även

- [Javscript filsystem - Node.js Dokumentation](https://nodejs.org/api/fs.html)
- [FileStream i Node.js - DigitalOcean Tutorial](https://www.digitalocean.com/community/tutorials/nodejs-reading-writing-files)
- [Öppna filer i Node.js - W3Schools Tutorial](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)