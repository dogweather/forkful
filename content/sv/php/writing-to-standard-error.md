---
title:                "PHP: Skrivning till standardfel"
simple_title:         "Skrivning till standardfel"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standard error är en viktig del av PHP-programmering. Det låter utvecklare utöva felsökning och diagnostisera problem som kan uppstå i koden. Det är också ett bra sätt att spåra meddelanden och händelser under körningen av en applikation.

## Hur man gör

För att skriva till standard error i PHP, använd funktionen `fwrite()` och ge som första argument `STDERR`. Detta ser till att meddelandet skrivs till standard error istället för standard output.
```
PHP
fwrite(STDERR, "Detta är ett felmeddelande");
```
Detta kommer att skriva ut "Detta är ett felmeddelande" till standard error.
```
PHP
php your_script.php 2> error_log.txt
```
Genom att ange `2> error_log.txt` som en del av kommandot kommer alla felmeddelanden att skrivas till en fil istället för att skrivas ut till terminalen.

## Djupdykning

Genom att använda `fwrite()` och `STDERR` på lämpliga ställen i koden kan vi få en bättre förståelse för eventuella problem som applikationen kan stöta på vid körning. Detta är särskilt användbart när man arbetar med stora och komplexa kodbaser.

Ett annat sätt att skriva till standard error är att använda funktionen `error_log()`. Detta låter utvecklare skriva till en specifik loggfil och ange loggnivån för meddelandet. Detta är särskilt användbart för att sortera och filtrera olika typer av felmeddelanden.

## Se också

- [PHP dokumentation om fwrite()](https://www.php.net/manual/en/function.fwrite.php)
- [PHP dokumentation om error_log()](https://www.php.net/manual/en/function.error-log.php)