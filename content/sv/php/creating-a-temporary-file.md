---
title:    "PHP: Skapa en temporär fil"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Varför?

Temporära filer är en viktig del av PHP-programmering eftersom de tillåter utbyte av data mellan olika delar av ett program. Genom att skapa temporära filer kan du enkelt lagra temporär data och sedan radera den när den inte längre behövs. Detta hjälper till att hålla din kod ren och förhindrar minnesläckor.

## Hur man gör

För att skapa en temporär fil i PHP kan du använda funktionen `tempnam()`. Den här funktionen tar två parametrar - sökvägen där den temporära filen ska skapas och ett prefix för filnamnet. Här är ett exempel på hur man använder `tempnam()`:

```PHP
$filepath = tempnam("/tmp", "TEMP");
echo "Temporär fil skapad på: " . $filepath;
```

Detta kommer att skapa en temporär fil med namnet "TEMP" i mappen "/tmp" och sedan skriva ut filvägen till denna fil. Om du inte anger någon sökväg kommer PHP att skapa filen i den temporära mappen för ditt system.

## Djupdykning

När du använder `tempnam()` är det viktigt att notera att filen som skapas faktiskt inte är tom. Den innehåller ett slumpmässigt genererat innehåll som kan användas för att identifiera filen och förhindra åtkomst av andra program.

Det är också viktigt att se till att du raderar den temporära filen när den inte längre behövs. Detta kan göras med funktionen `unlink()`. Om du inte raderar filen manuellt kommer den att raderas automatiskt när ditt PHP-program avslutas.

## Se även

För mer information om PHP-programmering och olika funktioner, kolla in följande länkar:

- https://www.php.net/manual/en/function.tempnam.php
- https://www.w3schools.com/php/func_filesystem_tempnam.asp
- https://www.geeksforgeeks.org/php-builder-pattern/
- https://www.phpclasses.org/blog/post/84-22-Easy-PHP-7-Code-Examples-of-Classes-and-Objects.html