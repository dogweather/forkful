---
date: 2024-01-26 01:07:14.081038-07:00
description: "Hur man g\xF6r: PHP kommer med en inbyggd felloggning funktion som \xE4\
  r enkel att anv\xE4nda. Bara stoppa in `error_log()` i din kod f\xF6r att skicka\
  \ ett meddelande\u2026"
lastmod: '2024-03-13T22:44:38.002800-06:00'
model: gpt-4-1106-preview
summary: "PHP kommer med en inbyggd felloggning funktion som \xE4r enkel att anv\xE4\
  nda."
title: Loggning
weight: 17
---

## Hur man gör:
PHP kommer med en inbyggd felloggning funktion som är enkel att använda. Bara stoppa in `error_log()` i din kod för att skicka ett meddelande till dina servertloggar. Du kan också anpassa den för att skriva till en specifik fil.

```php
<?php
// Loggar ett enkelt info-meddelande
error_log("Det här är en info loggpost.");

// Loggar ett felmeddelande
error_log("Det här är en error loggpost.", 0);

// Loggar till en angiven fil
file_put_contents('/sökväg/till/din/anpassade.log', "En anpassad loggpost.\n", FILE_APPEND);

// Använder Monolog för strukturerad loggning
require 'vendor/autoload.php';
use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// Skapa loggern
$logger = new Logger('namn');
// Lägg nu till några handlers
$logger->pushHandler(new StreamHandler('/sökväg/till/din/monolog.log', Logger::WARNING));

// Nu kan du använda din logger
$logger->warning('Det här är en varning logg!');
$logger->error('Det här är en error logg!');
?>
```

Detta kommer att skriva ut dina loggar till antingen serverloggen eller din angivna fil i klartextformat.

## Fördjupning:
Historiskt har PHP-utvecklare förlitat sig på `error_log()`-funktionen eller Apache/Nginx-loggarna för att fånga problem, men det kan vara kaotiskt med behovet av att tolka rena textfiler och inget enkelt sätt att filtrera eller sortera dem. Enter loggbibliotek som Monolog, som inledde eran av strukturerad loggning i PHP. Dessa lösningar ger dig bättre kontroll genom att erbjuda flera loggningskanaler, allvarlighetsnivåer och formaterad utdata (som JSON, vilket är en dröm för programmatisk tolkning).

Alternativ till Monolog inkluderar Log4php, KLogger och Apache's Log4php. När det gäller genomförande kräver robust loggning inte bara att data dumpas hur som helst, utan att man överväger saker som loggrotation, arkiveringsstrategier och integration med övervakningsverktyg för att verkligen vara användbara.

Du bör ha [PSR-3 Logger Interface](https://www.php-fig.org/psr/psr-3/) i åtanke, som skisserar ett gemensamt gränssnitt för loggbibliotek, vilket säkerställer samverkan och ett konsekvent sätt att få tillgång till loggningsmekanismer.

## Se även:
- [Monolog GitHub Repository](https://github.com/Seldaek/monolog)
- [PSR-3 Logger Interface Specification](https://www.php-fig.org/psr/psr-3/)
- [PHP Error Log Documentation](https://www.php.net/manual/en/function.error-log.php)
- [KLogger: En Enkel Loggklass För PHP](https://github.com/katzgrau/KLogger)
- [Log4php: Ett mångsidigt loggningsramverk för PHP](https://logging.apache.org/log4php/)

Börja med de inbyggda funktionerna, men för en mer underhållbar och skalbar metod, överväg att investera tid för att bli bekväm med ett bibliotek som Monolog. Lycklig loggning!
