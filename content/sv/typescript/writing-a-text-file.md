---
title:    "TypeScript: Skriva en textfil"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att kunna skriva en textfil är en grundläggande färdighet inom programmering. Genom att lära sig denna färdighet kan man spara data och information på ett enkelt sätt och återanvända det i olika delar av ett program. Det är även en viktig del av att bygga och utveckla appar, webbsidor och andra applikationer.

## Så här gör du

För att skriva en textfil i TypeScript behöver du först skapa en variabel som pekar på en textfil. Detta görs genom att använda funktionen `createWriteStream` från modulen `fs`. När du har skapat din variabel kan du sedan använda metoden `write` för att skriva text till filen. Se nedanstående kodexempel för att få en bättre förståelse:

```TypeScript
import { createWriteStream } from "fs";

// Skapar en variabel som pekar på textfilen 'textfil.txt'
let textFil = createWriteStream("textfil.txt");

// Skriver texten "Här är en exempeltext" till filen
textFil.write("Här är en exempeltext");

// Stänger filen och sparar ändringarna
textFil.close();
```

När du har kört koden ovan så kommer en textfil med namnet "textfil.txt" att skapas i samma mapp som din kodfil. Du kan öppna filen för att se att texten har skrivits till den.

## Djupdykning

För mer avancerade användare finns det olika metoder och tekniker för att skriva textfiler i TypeScript. Man kan till exempel använda sig av olika formatteringsverktyg för att göra textfilen mer läsbar, eller använda sig av loopar för att skriva större mängder data till filen. Det finns även andra moduler som kan importeras för att underlätta skrivandet av textfiler.

## Se även

- [TypeScript Docs - fs Modulen](https://www.typescriptlang.org/docs/handbook/file-system-utility.html)
- [Node.js Dokumentation - fs Modulen](https://nodejs.org/api/fs.html)