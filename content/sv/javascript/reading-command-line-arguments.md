---
title:                "Läsa kommandoradsargument"
html_title:           "Bash: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad och Varför?
"Inläsning av kommandoradsargument" innebär att interagera med argument som ges till din Node.js-applikation när den körs från kommandoraden. Detta är viktigt för att tillåta dynamiska värden i din kod, vilket gör din applikation mer flexibel och återanvändbar.

## Hur man gör:
För att läsa kommandoradsargument i Node.js, använder vi `process.argv`, vilket är en global variabel som innehåller en array av argument.

```Javascript
// Här är ett exempel
process.argv.forEach((val, index) => {
    console.log(`${index}: ${val}`);
});
```

När du kör detta script med argument från kommandoraden kommer terminalen att visa argumentens index och värde.

```Javascript
$ node test.js en två tre
0: /usr/local/bin/node
1: /Users/Användare/test.js
2: en
3: två
4: tre
```

## Djupdykning
Historiskt sett används interaktioner med kommandoraden fortfarande i utsträckning inom vissa områden, som för server-script, automatiseringsverktyg och utvecklingsarbetsflöden. 

Alternativt för att undvika process.argv komplexitet, finns tredjepartspaket som `yargs` eller `commander` för att göra arbetet smidigare och kodläsningen klarare.

När det gäller implementationen lagras `process.argv` som en array med datasträngar, där de två första elementen är standardsökvägar till Node.js och den körda filen. Resten av argumenten finns tillgängliga därefter.

## Se även
- Node.js Dokumentation (https://nodejs.org/api/process.html#process_process_argv)
- NPM yargs (https://www.npmjs.com/package/yargs)
- NPM commander (https://www.npmjs.com/package/commander)