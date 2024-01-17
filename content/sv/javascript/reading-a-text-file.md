---
title:                "Läsa en textfil"
html_title:           "Javascript: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa en textfil betyder att man tar in innehållet från en textfil och gör det åtkomligt för ditt program att bearbeta. Detta är användbart för att till exempel läsa in en lista över användarnamn eller importera data från en annan källa. Det kan användas för att automatisera uppgifter och göra processer mer effektiva för programmerare.

## Hur man gör:
Det finns flera sätt att läsa en textfil i Javascript, men en vanlig metod använder sig av inbyggda funktioner som "fs" (file system). Här är ett enkelt exempel på hur man kan läsa innehållet från en textfil och skriva ut det till konsolen:
```javascript
const fs = require('fs');
const data = fs.readFileSync('document.txt', 'utf8');

console.log(data);
```

Detta kodblock använder "fs" för att läsa in filen med namnet "document.txt" och lagrar innehållet i en variabel som heter "data". Sedan skrivs innehållet ut till konsolen med hjälp av console.log().

## Djupdykning:
Läsning av textfiler i programmering har funnits sedan långt tillbaka och är en fundamental del av filhantering. Idag finns det flera alternativ till att använda inbyggda funktioner som "fs", såsom att använda externa bibliotek eller webbläsarens FileReader API. Det är även viktigt att tänka på olika filformat och hur man hanterar eventuella fel som kan uppstå vid läsning av en textfil.

## Se även:
- [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- [https://developer.mozilla.org/en-US/docs/Web/API/FileReader](https://developer.mozilla.org/en-US/docs/Web/API/FileReader)
- [https://javascript.info/file](https://javascript.info/file)