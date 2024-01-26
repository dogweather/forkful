---
title:                "Skriva en textfil"
html_title:           "Arduino: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva en textfil innebär att du lagrar data i en läsbar form på disk. Programmerare gör detta för att spara konfigurationer, exportera data och logga händelser.

## Hur gör man:
I Node.js, använd `fs`-modulen för att skriva till en fil:

```javascript
const fs = require('fs');

fs.writeFile('exempel.txt', 'Hej världen!', function(err) {
    if(err) {
        return console.log(err);
    }
    console.log('Filen har sparats!');
});
```

Om du vill appenda till en fil istället för att skriva över den:

```javascript
fs.appendFile('exempel.txt', ' Hej igen!', function(err) {
    if(err) {
        return console.log(err);
    }
    console.log('Text tillagd till filen!');
});
```

## Djupdykning
Före Node.js, användes andra tekniker som PHP eller Python för filhantering på serversidan. Webbläsare har traditionellt inte haft möjlighet att skriva direkt till filsystemet. Node.js `fs`-modulen erbjuder ett `writeFile` för att skapa nya eller ersätta innehållet i befintliga filer och `appendFile` för att lägga till innehåll i en fil.

## Se även
- Node.js dokumentation om filsystemet: https://nodejs.org/api/fs.html
- MDN's guide till JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide
