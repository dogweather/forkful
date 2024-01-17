---
title:                "Skriva till standardfel"
html_title:           "PHP: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva till standard error är en programmeringsteknik som används för att skicka felmeddelanden till terminalen istället för till standard utmatningskanalen. Detta kan vara användbart för att hålla terminalen ren och för att få en tydligare feedback från programmet.

## Hur man gör:
För att skriva till standard error i PHP använder man funktionen `fwrite()` tillsammans med `$stderr` som en parameter. Detta gör det möjligt att skicka ett felmeddelande till terminalen, till exempel:

```PHP
<?php
    fwrite($stderr, "Ett fel har inträffat!");
?>
```

Detta kommer att skriva ut "Ett fel har inträffat!" i terminalen istället för i standard utmatningskanalen.

## Djupdykning:
Skrivning till standard error har funnits sedan början av Unix-system, men används fortfarande som en vanlig programmeringsteknik för att skicka felmeddelanden till terminalen. En alternativ metod är att skriva till en loggfil istället, men detta kan vara mindre praktiskt och kräver mer arbete vid debugging.

I PHP finns också funktionen `error_log()` som en alternativ metod för att skicka felmeddelanden till en loggfil. Detta kan vara mer användbart vid långsiktiga projekt eller för att samla alla felmeddelanden på en central plats.

## Se även:
Läs mer om `fwrite()` funktionen i PHP:s officiella dokumentation: https://www.php.net/manual/en/function.fwrite.php