---
title:    "PHP: Skriva till standardfel"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Varför
Att skriva till standard error, även känt som STDERR, är en viktig del av programmering för att spåra och åtgärda fel och buggar i din kod. Genom att skriva till STDERR kan du separera felmeddelanden från vanlig output och göra det lättare att felsöka ditt program.

## Hur man gör
För att skriva till STDERR i PHP, använder du funktionen `fwrite()`. Du behöver också öppna filen för att skriva till den. Se till att du öppnar filen med skrivbehörighet för att undvika eventuella felmeddelanden.

```PHP
$myfile = fopen("error.log", "w");
fwrite($myfile, "Detta är ett felmeddelande som skrivits till STDERR.");
fclose($myfile);
```

När du kör detta program kommer ett felmeddelande att skrivas till filen `error.log` istället för att visas på websidan. Detta kan hjälpa till att hålla din output ren och enkel att läsa.

## Djupdykning
Skrivande till STDERR är också användbart när du vill logga felmeddelanden till en fil istället för att visa dem för användaren. Du kan till exempel använda STDERR för att spåra fel i en inloggningssida och sedan logga dessa felmeddelanden till en fil som bara administratörer kan se.

En annan fördel med att skriva till STDERR är att du kan använda verktyg som `tail` för att övervaka loggar i realtid. Detta kan vara särskilt användbart när du utvecklar eller felsöker din kod.

## Se även
- [PHP fwrite() funktionen](https://www.php.net/manual/en/function.fwrite.php)
- [Felsökningsguide för PHP](https://www.php.net/manual/en/book.errorfunc.php)
- [Tail command i Linux](https://www.geeksforgeeks.org/tail-command-linux/)