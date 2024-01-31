---
title:                "Skriva till standardfel"
date:                  2024-01-19
html_title:           "Arduino: Skriva till standardfel"
simple_title:         "Skriva till standardfel"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skrivning till standardfel (stderr) separerar felmeddelanden från vanlig utdata. Programmers gör det för att hantera fel effektivt och underlätta felsökning.

## Hur Gör Man:
För att skriva till stderr i PHP används `fwrite()` mot `php://stderr`.

```PHP
<?php
// Skicka ett felmeddelande till standardfel.
fwrite(STDERR, "Ett fel har uppstått!\n");
?>
```

Exempelutdata i terminalen skulle kunna se ut så här efter att ha körts:

```
Ett fel har uppstått!
```

## Djupdykning:
`STDERR` är en inbyggd filbeskrivare i PHP som är tillgänglig utan att öppna en ström manuellt. Historiskt sett kommer konceptet med standardfel från Unix-operativsystemen, där stderr används för att skilja vanlig utdata från errorutdata. Alternativt kan man använda `error_log()` för att skicka felmeddelanden direkt till den förkonfigurerade loggen. Implementationen av stderr i PHP följer principer etablerade i C's stdio-bibliotek.

## Se även:
- PHP:s officiella dokumentation om felhantering: https://www.php.net/manual/en/book.errorfunc.php
- PHP:s officiella dokumentation om de olika I/O-strömmarna: https://www.php.net/manual/en/wrappers.php.php
- Mer om standardströmmar i Unix: https://en.wikipedia.org/wiki/Standard_streams
