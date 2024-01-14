---
title:    "Javascript: Läsa en textfil"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att läsa en textfil är en mycket användbar färdighet för alla som programmerar i Javascript. Det gör det möjligt att lagra och läsa in data på ett strukturerat sätt och kan hjälpa till med att lösa många programmeringsproblem.

## Hur man gör

För att läsa en textfil i Javascript behöver du först skapa en instans av ett filsystem-objekt genom att använda `require('fs')` metoden. Därefter kan du använda `readFile()` metoden för att läsa in filen och lagra den i en variabel.

```Javascript
let fs = require('fs');
let data = fs.readFileSync('textfil.txt', 'utf8'); 
```

Den första parametern i `readFile()` är namnet på den fil som ska läsas in och den andra parameter är teckenkodningen för filen (i detta fall är det "utf8"). Den lästa filen sparas i variabeln `data`.

För att konvertera den lästa filen till en array, kan du använda `.split()` metoden:

```Javascript
let dataArr = data.split('\n');
```

Detta kommer att dela upp filen vid varje radbrytning och lagra den som en array. Om du istället vill läsa in en CSV-fil och konvertera den till en array av objekt, kan du använda en loop tillsammans med `.split()` metoden för att dela upp datan och skapa objekt med de olika värdena:

```Javascript
let dataObj = [];
for (let i = 0; i < dataArr.length; i++) {
    let row = dataArr[i].split(',');
    let obj = {
        name: row[0],
        age: row[1],
        city: row[2]
    };
    dataObj.push(obj);
}
```

Detta kommer att läsa in en CSV-fil där varje rad innehåller ett namn, en ålder och en stad och konvertera den till en array av objekt.

Slutligen kan du använda `.writeFile()` metoden för att skriva ut data till en textfil:

```Javascript
let newData = "Hello world!";
fs.writeFile('nyfil.txt', newData, (err) => {
    if (err) throw err;
    console.log('Filen har skapats!');
});
```

## Deep Dive (Djupdykning)

När du läser in en textfil i Javascript, är det viktigt att vara medveten om teckenkodningen på filen. Om filen har en annan teckenkodning än "utf8", måste du ange den korrekta teckenkodningen i `readFile()` metoden för att få korrekta resultat.

Det kan också vara viktigt att hantera eventuella fel som kan uppstå vid inläsning och skrivning av filer. Det är därför som i exempelkoden ovan använder vi `try-catch` block för att fånga eventuella fel som kan uppkomma.

## Se även

- [Node.js file system](https://nodejs.org/api/fs.html)
- [Dokumentation för fs.readFileSync()](https://nodejs.org/api/fs.html#fs_fs_readfilesync_path_options)
- [Dokumentation för fs.writeFile()](https://nodejs.org/api/fs.html#fs_fs_writefile_file_data_options_callback)
- [Dokumentation för fs.Split()](https://nodejs.org/api/fs.html#fs_fs_readfilesync_path_options)