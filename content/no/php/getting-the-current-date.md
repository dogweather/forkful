---
title:                "Slik får du tak i dagens dato"
date:                  2024-01-20T15:15:53.453818-07:00
html_title:           "C: Slik får du tak i dagens dato"
simple_title:         "Slik får du tak i dagens dato"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Vi henter dagens dato for å merke hendelser, lagre tidsavhengig data, eller generere tidsbaserte rapporter. Å vite "når" er kritisk i mange applikasjoner, fra blogginnlegg til transaksjonslogger.

## Hvordan gjøre det:
```PHP
<?php
echo date("Y-m-d") . "\n"; // Format: ÅÅÅÅ-MM-DD
echo date(DATE_ATOM) . "\n"; // ISO 8601 format
?>
```
Sample Output:
```
2023-04-07
2023-04-07T12:45:00+02:00
```

## Dypdykk
PHPs `date()` funksjonen har eksistert siden de tidligste dagene, tilbake til versjon 2.0 i 1996. 

Alternativer: `DateTime` klasse ble introdusert i PHP 5.2 for å tilby mer robust dato-tid funksjonalitet, inkludert tidssonehåndtering.
```PHP
$datetime = new DateTime();
echo $datetime->format('Y-m-d H:i:s');
```

Implementeringsdetaljer: Serverens tidssoneinnstillinger påvirker `date()`, bruk `date_default_timezone_set()` for å justere det etter behov. Etter PHP 7, fikk vi `DateTimeImmutable` som tillater "verdifri" dato-tid representasjon, unngå utilsiktet modifikasjon.

## Se også
- PHPs offisielle dokumentasjon på `date()` funksjonen: https://www.php.net/manual/en/function.date.php
- Tidssoner og PHP, en umiddelbar guide: https://www.php.net/manual/en/datetime.configuration.php
- `DateTime` klasse: https://www.php.net/manual/en/class.datetime.php
