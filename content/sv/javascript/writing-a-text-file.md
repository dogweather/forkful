---
title:                "Javascript: Skriva en textfil"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Varför

Att kunna skriva en textfil är en grundläggande färdighet som behövs inom webbutveckling. Det är ett sätt att lagra och organisera information som kan vara användbart i många olika situationer, till exempel att spara användarinställningar, loggar eller innehåll till en webbsida.

# Hur man gör

För att skriva en textfil i Javascript behöver man först och främst ha en kodredigerare eller en texteditor såsom Visual Studio Code eller Sublime Text. Sedan kan man använda sig av Javascripts inbyggda "fs" modul för att skapa en textfil. Genom att använda funktionen "writeFileSync" kan man skriva en textsträng till en given fil. Se nedan för ett enkelt exempel:

```Javascript
const fs = require('fs');

fs.writeFileSync("mittexempel.txt", "Det här är min första textfil!");
```

I detta exempel skapar vi en fil som heter "mittexempel.txt" och skriver texten "Det här är min första textfil!" till den. Om man öppnar filen så kommer man att se att texten har skrivits korrekt.

# Djupdykning

När man skriver en textfil i Javascript finns det flera olika parametrar som man kan använda för att styra hur filen skrivs och vilken information som ska skrivas till den. Till exempel kan man ange en "flagga" som avgör om det ska skrivas ny information till filen eller om det bara ska läggas till texten till slutet av filen. Man kan också ange en "encoding" som bestämmer vilken typ av teckenkodning som ska användas för att läsa eller skriva filen.

En annan viktig aspekt av att skriva en textfil är att se till att filen stängs efteråt. Om man inte stänger filen kan det leda till buggar och minnesläckor. För att undvika detta kan man använda funktionen "closeSync" för att stänga filen när man är klar med den.

# Se även

- [Javascripting tutorial: Writing files](https://github.com/workshopper/javascripting#writing-files)
- [Node.js Docs: fs module](https://nodejs.org/api/fs.html)
- [Codecademy: fs module](https://www.codecademy.com/en/courses/learn-node-sys-admin/lessons/writing-files/exercises/fs-module)