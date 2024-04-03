---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:17.334981-07:00
description: "Hur man g\xF6r: I PHP kan man skriva till stderr genom att anv\xE4nda\
  \ funktionen `fwrite()` tillsammans med den f\xF6rdefinierade konstanten `STDERR`,\
  \ som\u2026"
lastmod: '2024-03-13T22:44:38.012546-06:00'
model: gpt-4-0125-preview
summary: "I PHP kan man skriva till stderr genom att anv\xE4nda funktionen `fwrite()`\
  \ tillsammans med den f\xF6rdefinierade konstanten `STDERR`, som representerar str\xF6\
  mmen f\xF6r felutdata."
title: Skriva till standardfel
weight: 25
---

## Hur man gör:
I PHP kan man skriva till stderr genom att använda funktionen `fwrite()` tillsammans med den fördefinierade konstanten `STDERR`, som representerar strömmen för felutdata.

```php
<?php
// Skriver ett enkelt meddelande till stderr.
fwrite(STDERR, "Det här är ett felmeddelande.\n");
```

Exempelutdata när skriptet körs från kommandotolken:
```
Det här är ett felmeddelande.
```

För att demonstrera en mer praktisk användning, tänk dig ett scenario där du analyserar användarinput och stöter på oväntade data:
```php
<?php
$input = 'oväntade data';

// Simulerar ett fel vid bearbetning av användarinput.
if ($input === 'oväntade data') {
    fwrite(STDERR, "Fel: Oväntad input mottagen.\n");
    exit(1); // Avslutar med ett icke-noll värde för att indikera ett fel.
}
```

Även om PHP:s inbyggda möjligheter att hantera stderr generellt är tillräckliga, när man arbetar med mer komplexa applikationer eller vill integrera stderr-loggning med externa system, kan tredjepartsbibliotek som Monolog vara en kraftfull allierad. Monolog är ett loggningsbibliotek som kan hantera stderr bland många andra mål (filer, sockets, etc.).

Att använda Monolog för att skriva till stderr:

Först, se till att du har installerat Monolog via Composer:
```
composer require monolog/monolog
```

Sedan kan du konfigurera Monolog att använda `StreamHandler` riktad mot `php://stderr`:

```php
<?php
require 'vendor/autoload.php';

use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// Skapar en loggkanal
$log = new Logger('namn');
$log->pushHandler(new StreamHandler('php://stderr', Logger::WARNING));

// Lägger till ett loggmeddelande till stderr
$log->warning('Det här är ett varningsmeddelande.');
```

Koden ovan använder Monolog för att skicka ett varningsmeddelande till stderr, vilket är särskilt användbart för applikationer som kräver detaljerade loggningskonfigurationer eller extern loggövervakning.
