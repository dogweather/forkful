---
title:    "Javascript: Att läsa en textfil"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Varför

Att kunna läsa en textfil är en viktig färdighet för programmörer. Det gör det möjligt för oss att hantera och visualisera stora mängder data på ett snabbt och effektivt sätt. Det är en grundläggande del av många programmeringsprojekt och ett värdefullt verktyg att ha i sin verktygslåda.

## Hur man gör

Att läsa en textfil i Javascript är ganska enkelt. Vi börjar med att skapa en variabel som lagrar sökvägen till vår textfil. Sedan använder vi 'fs' modulen för att läsa filen och skriva ut den till konsolen.

```Javascript
let filePath = 'textfil.txt';
let fs = require('fs');

fs.readFile(filePath, 'utf8', (err, data) => {
    if (err) throw err;
    console.log(data);
});
```

Vi använder 'fs.readFile' funktionen och anger sökvägen till filen, teckenkodningen och en callback-funktion som utför vår utskrift.

I detta exempel har vi antagit att vår textfil är i samma mapp som vårt Javascript-fil. Men om filen finns någon annanstans behöver vi ange hela sökvägen.

## Djupdykning

När vi använder funktionen 'fs.readFile' är det viktigt att förstå att det är en asynkron funktion. Det betyder att den inte blockera andra operationer medan filen läses. Istället körs det parallellt med resten av vår kod, vilket är en fördel i många situationer.

En annan viktig sak att notera är att vår callback-funktion tar emot två parametrar, 'error' och 'data'. Om det finns ett fel kommer detta att vara den första parametern och om det inte finns något fel så kommer vår fildata att vara den andra parametern.

Vi kan också använda funktionen 'fs.readFileSync' som är en synkron funktion. Det betyder att den kommer att blockera resten av vår kod tills filen läses helt. Detta kan vara användbart i vissa situationer, men i allmänhet är det bättre att använda den asynkrona funktionen för att undvika att vår kod blockeras.

## Se även

- [Node.js filsystemsdokumentation](https://nodejs.org/api/fs.html)
- [W3Schools guide om att läsa filer i Node.js](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)
- [Tutorialspoint artikel om att läsa och skriva textfiler i Node.js](https://www.tutorialspoint.com/nodejs/nodejs_filesystem.htm)