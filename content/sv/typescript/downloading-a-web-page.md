---
title:                "Ladda ner en webbsida"
html_title:           "TypeScript: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Varför

Om du är intresserad av webbutveckling och vill lära dig ett kraftfullt programmeringsspråk, kan det vara värt att ta en titt på TypeScript. Det är ett modernt objektorienterat språk som kompilerar till ren JavaScript-kod och ger utvecklare möjlighet att skriva kod på ett mer strukturerat och robust sätt.

## Hur man gör

För att använda TypeScript för att ladda ner en webbsida behöver du först installera Node.js och NPM på din dator. När det väl är installerat kan du enkelt installera TypeScript genom kommandot `npm install -g typescript`. Sedan kan du följa dessa steg för att ladda ner en webbsida:

1. Skapa en ny mapp och öppna den i din terminal eller kommandoprompt.
2. Skapa en ny fil, till exempel `downloader.ts`, och öppna den i din favoritkodredigerare.
3. Skriv följande kod i filen:

```TypeScript
import * as fs from 'fs';
import * as https from 'https';

// Ladda ner webbsida och spara den som en textfil
https.get('https://www.example.com', (response) => {
  response.setEncoding('utf8');

  // Skapa en ström från responsen för att skriva till en fil
  const writeStream = fs.createWriteStream('output.html');

  // Skriv responsen till vår fil
  response.on('data', (chunk) => {
    writeStream.write(chunk);
  });

  // När all data har skrivits klart stänger vi strömmen och sparar filen
  response.on('end', () => {
    writeStream.end();
    console.log('Webbsida laddad ner och sparad som "output.html"');
  });
}).on('error', (error) => {
  console.error(`Fel vid nedladdning av webbsida: ${error.message}`);
});
```

4. Spara filen och gå tillbaka till din terminal eller kommandoprompt.
5. Kör kommandot `tsc downloader.ts` för att kompilera filen till JavaScript.
6. Kör sedan kommandot `node downloader.js` för att köra din kod.

Du borde nu ha en ny fil som heter "output.html" i din mapp, som innehåller den nedladdade webbsidan.

## Djupdykning

I exemplet ovan använde vi `https`-modulen för att skicka en HTTP-begäran till webbsidan och hämta responsen. Vi använde också `fs`-modulen för att skapa en ström och skriva responsen till en fil. Det finns många andra funktioner som kan användas för att anpassa din kod och lägga till funktioner som hantering av fel, parsning av HTML och mycket mer.

## Se även

- [Officiell TypeScript-dokumentation](https://www.typescriptlang.org/docs/)
- [Node.js-dokumentation](https://nodejs.org/en/docs/)