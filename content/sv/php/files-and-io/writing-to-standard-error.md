---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:17.334981-07:00
description: "Att skriva till standardfel (stderr) i PHP handlar om att styra felmeddelanden\
  \ eller diagnostik separat fr\xE5n standardutdata (stdout), vilket g\xF6r det\u2026"
lastmod: '2024-03-11T00:14:11.387005-06:00'
model: gpt-4-0125-preview
summary: "Att skriva till standardfel (stderr) i PHP handlar om att styra felmeddelanden\
  \ eller diagnostik separat fr\xE5n standardutdata (stdout), vilket g\xF6r det\u2026"
title: Skriva till standardfel
---

{{< edit_this_page >}}

## Vad och varför?

Att skriva till standardfel (stderr) i PHP handlar om att styra felmeddelanden eller diagnostik separat från standardutdata (stdout), vilket gör det möjligt för utvecklare att bättre hantera sina utdataströmmar för felsökning och loggning. Programmerare använder denna teknik för att säkerställa att felmeddelanden inte stör programmets utdata, vilket gör det enklare att övervaka och felsöka applikationer.

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
