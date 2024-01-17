---
title:                "Å beregne en dato i fremtiden eller fortiden"
html_title:           "PHP: Å beregne en dato i fremtiden eller fortiden"
simple_title:         "Å beregne en dato i fremtiden eller fortiden"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å beregne en dato i fremtiden eller fortiden er en vanlig oppgave for programmerere. Dette kan være nyttig for å vise datoer i en kalender, lage alderskalkulatorer og mye mer.

## Slik gjør du det:
Det er flere måter å beregne en dato i PHP. En enkel måte er å bruke funksjonen `strtotime`, som tar et datostreng og returnerer en dato. For eksempel:

```PHP
<?php
echo date('d.m.Y', strtotime('+1 day')); // Skriver ut datoen i morgen
echo date('d.m.Y', strtotime('-1 week')); // Skriver ut datoen for en uke siden
```

Dette vil gi output som følger:

```01.01.2022
25.12.2021
```

En annen måte å beregne datoer på er å bruke `DateTime`-klassen. Denne gir mulighet for å utføre mer komplekse operasjoner med datoer, som å legge til eller trekke fra flere dager. Her er et eksempel:

```PHP
<?php
$today = new DateTime();
$futureDate = $today->modify('+2 months'); // Legger til to måneder til dags dato
echo $futureDate->format('d.m.Y'); // Skriver ut den fremtidige datoen
```

Dette vil gi output som følger:

```03.03.2022
```

## Grundigere forklaring:
Beregning av datoer i programmering har sin opprinnelse i kalenderen. Tidligere ble dette gjort manuelt, men med utviklingen av datamaskiner ble dette automatisert. Alternativer til å beregne datoer i fremtiden eller fortiden i PHP inkluderer å bruke `mktime`-funksjonen og `DateTimeImmutable`-klassen.

Når du beregner en dato i PHP, kan du også bruke forskjellige formater for å få ønsket utseende på resultatet. Dette kan du gjøre ved å bruke `date`-funksjonen. For mer informasjon og flere eksempler, kan du se dokumentasjonen for PHP om dette emnet.

## Se også:
- [PHP date funksjon](https://www.php.net/manual/en/function.date.php)
- [PHP DateTime klasse](https://www.php.net/manual/en/class.datetime.php)
- [Alternative måter å beregne datoer i PHP](https://www.php.net/manual/en/datetime.formats.date.php)