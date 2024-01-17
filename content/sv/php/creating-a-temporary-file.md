---
title:                "Skapa en temporär fil"
html_title:           "PHP: Skapa en temporär fil"
simple_title:         "Skapa en temporär fil"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skapande av en tillfällig fil är en vanlig teknik som används av programmerare för att tillfälligt lagra data eller utföra operationer på en fil utan att påverka den permanenta versionen. Det är särskilt användbart för att hantera stora mängder data eller när man behöver skapa temporära filer för ett specifikt syfte.

## Hur gör man:
Skapandet av en temporär fil i PHP är enkelt. Det finns två huvudsakliga funktioner som kan användas för detta ändamål: `tempnam()` och `tmpfile()`. Den första funktionen skapar en tillfällig fil och returnerar filens sökväg, medan den andra returnerar en temporär filressurs. Här är en enkel kodexempel för att skapa en tillfällig fil med `tempnam()`:

```PHP
$tempFileName = tempnam('/tmp', 'prefix_');
echo $tempFileName; // /tmp/prefix_a1b2c3
```

I det här exemplet skapas en tillfällig fil i mappen `/tmp` med prefixet "prefix_". Funktionen `tempnam()` genererar en unik sökväg för filen baserat på den angivna mappen och prefixet. Om du istället vill använda `tmpfile()` funktionen, kan du göra så här:

```PHP
$tempFile = tmpfile();
echo get_resource_type($tempFile); // file
```

I det här fallet behöver du inte ange en sökväg eller ett prefix, eftersom funktionen `tmpfile()` skapar en tillfällig fil i systemets temporära mapp och returnerar en filressurs.

## Djupdykning:
Skapandet av tillfälliga filer har funnits sedan begynnelsen av datorprogrammering. Det användes främst för att hantera stora datamängder innan hantering av databaser blev mer utbredd. Idag används det fortfarande för att hantera data som inte passar i en traditionell databas eller för att utföra snabba filoperationer.

Alternativ till att skapa temporära filer inkluderar att använda en minnesbuffert eller en temporär databas. Detta kan vara mer effektivt beroende på syftet med den temporära filen och dess storlek.

När en fil skapas med `tempnam()` behöver du se till att ta bort filen när du är klar med att använda den. Detta görs vanligtvis med funktionen `unlink()`. Om du använder `tmpfile()` funktionen kommer filen att tas bort automatiskt när skriptet avslutas.

## Se även:
- [PHP Manual - tempnam](https://www.php.net/manual/en/function.tempnam.php)
- [PHP Manual - tmpfile](https://www.php.net/manual/en/function.tmpfile.php)
- [PHP Manual - unlink](https://www.php.net/manual/en/function.unlink.php)