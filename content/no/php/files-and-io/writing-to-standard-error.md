---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:05.947061-07:00
description: "\xC5 skrive til standardfeil (stderr) i PHP handler om \xE5 dirigere\
  \ feilmeldinger eller diagnostikk separat fra standard ut (stdout). Dette lar utviklere\
  \ bedre\u2026"
lastmod: '2024-03-13T22:44:40.902744-06:00'
model: gpt-4-0125-preview
summary: "\xC5 skrive til standardfeil (stderr) i PHP handler om \xE5 dirigere feilmeldinger\
  \ eller diagnostikk separat fra standard ut (stdout)."
title: Skriving til standardfeil
weight: 25
---

## Hva & Hvorfor?

Å skrive til standardfeil (stderr) i PHP handler om å dirigere feilmeldinger eller diagnostikk separat fra standard ut (stdout). Dette lar utviklere bedre håndtere sine utstrømsdata for feilsøking og logging. Programmerere bruker denne teknikken for å sikre at feilmeldinger ikke forstyrrer programmets utdata, noe som gjør det enklere å overvåke og feilsøke applikasjoner.

## Hvordan:

I PHP kan skriving til stderr oppnås ved å bruke funksjonen `fwrite()` sammen med den forhåndsdefinerte konstanten `STDERR`, som representerer feilutstrømmen.

```php
<?php
// Skriver en enkel melding til stderr.
fwrite(STDERR, "Dette er en feilmelding.\n");
```

Eksempel på utdata når skriptet kjøres fra kommandolinjen:
```
Dette er en feilmelding.
```

For å demonstrere mer praktisk bruk, tenk på et scenario hvor du analyserer brukerinndata og støter på uventede data:
```php
<?php
$input = 'uventede data';

// Simulerer en feil ved behandling av brukerinndata.
if ($input === 'uventede data') {
    fwrite(STDERR, "Feil: Uventet input mottatt.\n");
    exit(1); // Avslutter med en ikke-null verdi for å indikere en feil.
}
```

Selv om PHPs innebygde funksjonalitet for å håndtere stderr generelt er tilstrekkelig, når man har å gjøre med mer komplekse applikasjoner eller ønsker å integrere stderr-logging med eksterne systemer, kan tredjepartsbiblioteker som Monolog være en kraftfull alliert. Monolog er et loggingbibliotek som kan håndtere stderr blant mange andre mål (filer, sokler, osv.).

Bruker Monolog for å skrive til stderr:

Først, sørg for at du har installert Monolog via Composer:
```
composer require monolog/monolog
```

Deretter kan du konfigurere Monolog til å bruke `StreamHandler` målrettet mot `php://stderr`:

```php
<?php
require 'vendor/autoload.php';

use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// Oppretter en loggkanal
$log = new Logger('navn');
$log->pushHandler(new StreamHandler('php://stderr', Logger::WARNING));

// Legger til en loggmelding til stderr
$log->warning('Dette er en advarselsmelding.');
```

Koden ovenfor bruker Monolog for å sende en advarselsmelding til stderr, noe som er spesielt nyttig for applikasjoner som krever detaljerte loggkonfigurasjoner eller ekstern loggovervåking.
