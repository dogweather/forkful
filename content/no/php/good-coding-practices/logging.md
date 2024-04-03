---
date: 2024-01-26 01:07:49.412606-07:00
description: "Hvordan gj\xF8re det: PHP kommer med en innebygd funksjon for feillogging\
  \ som er enkel \xE5 bruke. Bare putt `error_log()` inn i koden din for \xE5 sende\
  \ en\u2026"
lastmod: '2024-03-13T22:44:40.893122-06:00'
model: gpt-4-1106-preview
summary: "PHP kommer med en innebygd funksjon for feillogging som er enkel \xE5 bruke."
title: "Loggf\xF8ring"
weight: 17
---

## Hvordan gjøre det:
PHP kommer med en innebygd funksjon for feillogging som er enkel å bruke. Bare putt `error_log()` inn i koden din for å sende en melding til serverloggene dine. Du kan også tilpasse det for å skrive til en bestemt fil.

```php
<?php
// Logger en enkel informasjonsmelding
error_log("Dette er en info loggoppføring.");

// Logger en feilmelding
error_log("Dette er en feil loggoppføring.", 0);

// Logger til en angitt fil
file_put_contents('/sti/til/din/custom.log', "En tilpasset loggoppføring.\n", FILE_APPEND);

// Bruker Monolog for strukturert logging
require 'vendor/autoload.php';
use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// Opprett loggeren
$logger = new Logger('navn');
// Nå legg til noen behandlere
$logger->pushHandler(new StreamHandler('/sti/til/din/monolog.log', Logger::WARNING));

// Du kan nå bruke loggeren din
$logger->warning('Dette er en advarsel logg!');
$logger->error('Dette er en feil logg!');
?>
```

Dette vil skrive ut loggene dine til enten serverloggen eller din spesifiserte fil i ren tekstformat.

## Dypdykk:
Historisk sett har PHP-utviklere stolt på `error_log()`-funksjonen eller Apache/Nginx-loggene for å fange opp problemer, men det kan være kaotisk med behovet for å analysere rene tekstfiler og ingen enkel måte å filtrere eller sortere dem på. Entrer loggingbiblioteker som Monolog, som innledet æraen av strukturert logging i PHP. Disse løsningene gir deg bedre kontroll ved å tilby flere loggingkanaler, alvorsnivåer og formatert utdata (som JSON, som er en drøm for programmatisk parsing).

Alternativer til Monolog inkluderer Log4php, KLogger og Apaches Log4php. Når det gjelder gjennomføring, krever robust logging ikke bare å dumpe data hvor som helst, men å vurdere ting som loggrotasjon, arkiveringsstrategier og integrasjon med overvåkningsverktøy for å virkelig være nyttig.

Du bør ha [PSR-3 Logger Interface](https://www.php-fig.org/psr/psr-3/) i bakhodet, som skisserer et felles grensesnitt for loggingbiblioteker, som sikrer interoperabilitet og en konsistent måte å få tilgang til loggingmekanismer på.

## Se også:
- [Monolog GitHub Repository](https://github.com/Seldaek/monolog)
- [PSR-3 Logger Interface Specification](https://www.php-fig.org/psr/psr-3/)
- [PHP Error Log Documentation](https://www.php.net/manual/en/function.error-log.php)
- [KLogger: En Enkel Logging Klasse for PHP](https://github.com/katzgrau/KLogger)
- [Log4php: Et fleksibelt loggingrammeverk for PHP](https://logging.apache.org/log4php/)

Begynn med de innebygde funksjonene, men for en mer vedlikeholdbar og skalerbar tilnærming, vurder å bruke tid på å bli komfortabel med et bibliotek som Monolog. God logging!
