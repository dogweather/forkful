---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:06.655131-07:00
description: "Loggen is in feite te vergelijken met het bijhouden van een dagboek\
  \ voor je code; het is het registreren van gebeurtenissen, fouten en andere significante\u2026"
lastmod: '2024-03-13T22:44:50.902022-06:00'
model: gpt-4-0125-preview
summary: "Loggen is in feite te vergelijken met het bijhouden van een dagboek voor\
  \ je code; het is het registreren van gebeurtenissen, fouten en andere significante\u2026"
title: Logboekregistratie
weight: 17
---

## Wat & Waarom?

Loggen is in feite te vergelijken met het bijhouden van een dagboek voor je code; het is het registreren van gebeurtenissen, fouten en andere significante gegevenspunten die plaatsvinden wanneer je applicatie draait. Programmeurs doen dit om bij te houden wat er onder de motorkap gebeurt, problemen te debuggen en een audit trail te onderhouden voor latere analyse of nalevingsdoeleinden.

## Hoe te:

PHP bevat een ingebouwde foutenlogfunctie die eenvoudig te gebruiken is. Plaats gewoon `error_log()` in je code om een bericht naar je serverlogs te sturen. Je kunt het ook aanpassen om naar een specifiek bestand te schrijven.

```php
<?php
// Een eenvoudig informatiebericht loggen
error_log("Dit is een informatielogboekvermelding.");

// Een foutbericht loggen
error_log("Dit is een foutlogboekvermelding.", 0);

// Loggen naar een gespecificeerd bestand
file_put_contents('/pad/naar/jouw/custom.log', "Een aangepaste logboekvermelding.\n", FILE_APPEND);

// Monolog gebruiken voor gestructureerd loggen
require 'vendor/autoload.php';
use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// Maak de logger
$logger = new Logger('naam');
// Voeg nu enkele handlers toe
$logger->pushHandler(new StreamHandler('/pad/naar/jouw/monolog.log', Logger::WARNING));

// Je kunt nu je logger gebruiken
$logger->warning('Dit is een waarschuwingslog!');
$logger->error('Dit is een foutlog!');
?>
```

Dit zal je logs uitvoeren naar het serverlogboek of je gespecificeerde bestand in platte tekstformaat.

## Diepere Duik:

Historisch gezien vertrouwden PHP-ontwikkelaars op de `error_log()` functie of de Apache/Nginx logs om problemen op te vangen, maar dat kan chaotisch zijn met de noodzaak om platte tekstbestanden te parsen en geen gemakkelijke manier om ze te filteren of sorteren. Toen kwamen logboekbibliotheken zoals Monolog, die het tijdperk van gestructureerd loggen in PHP inluidden. Deze oplossingen geven je meer controle door meerdere logkanalen, ernstniveaus en geformatteerde uitvoer (zoals JSON, wat een droom is voor programmatisch parsen) te bieden.

Alternatieven voor Monolog zijn onder andere Log4php, KLogger en Apache's Log4php. Wat betreft de implementatie, vereist robuust loggen niet alleen willekeurig gegevens dumpen, maar ook rekening houden met zaken zoals logrotatie, archiveringsstrategieën en integratie met monitoringtools om echt nuttig te zijn.

Je moet de [PSR-3 Logger Interface](https://www.php-fig.org/psr/psr-3/) in gedachten houden, die een gemeenschappelijke interface voor logboekbibliotheken schetst, waardoor interoperabiliteit en een consistente manier om toegang te krijgen tot logmechanismen gewaarborgd zijn.

## Zie ook:

- [Monolog GitHub Repository](https://github.com/Seldaek/monolog)
- [PSR-3 Logger Interface Specificatie](https://www.php-fig.org/psr/psr-3/)
- [PHP Foutenlog Documentatie](https://www.php.net/manual/en/function.error-log.php)
- [KLogger: Een Eenvoudige Logklasse Voor PHP](https://github.com/katzgrau/KLogger)
- [Log4php: Een veelzijdig logkader voor PHP](https://logging.apache.org/log4php/)

Begin met de ingebouwde functies, maar voor een meer onderhoudbare en schaalbare aanpak, overweeg om tijd te investeren om comfortabel te worden met een bibliotheek zoals Monolog. Gelukkig loggen!
