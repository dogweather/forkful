---
title:                "Tolke en dato fra en streng"
html_title:           "Bash: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Å Analysere en Dato fra en Streng i PHP 

## Hva & Hvorfor?

Å analysere en dato fra en streng er prosessen å konvertere en datostreng til et mer håndterbart format som et `DateTime`-objekt. Programmerere gjør dette for å kunne manipulere og utføre operasjoner med datoen.

## Hvordan til:

La oss dykke rett inn i hvordan vi kan gjøre dette i PHP:

```PHP
<?php
$datostreng = '21-10-2021';
$dato = DateTime::createFromFormat('d-m-Y', $datostreng);

echo $dato->format('Y-m-d');
?>
```

Når du kjører koden over vil det utskrifte `2021-10-21`.

## Dyp Dykk 

Parse en dato fra en streng i PHP er ganske greit takket vært `DateTime::createFromFormat` funksjon. Denne funksjonen ble introdusert i PHP 5.3.0, forenklet prosessen med å jobbe med datoer i stor grad. Før det, måtte kodere stole på `strtotime()`, som kunne være unøyaktig og uforutsigbar. 

Som alternativer til PHP, tilbyr mange andre programmeringsspråk, som Python og Javascript, også deres egne metoder for datoparsering, som `strptime()` og `Date.parse()` henholdsvis. 

Husk at når du bruker PHP's `DateTime::createFromFormat`, må du spesifisere formatet til datostrengen du prøver å analysere. Hvis strengen ikke samsvarer med formatet du har gitt, vil PHP returnere `false`.

## Se Også

Hvis du ønsker ytterligere informasjon om håndtering av datoer og tider i PHP, sjekk ut følgende ressurser: 

1. PHP Manual: DateTime (https://www.php.net/manual/en/class.datetime.php) 
2. PHP: Dates - Manual (https://www.php.net/manual/en/datetime.formats.compound.php)
3. Understanding Date and Time in PHP - PHPBuilder (https://www.phpbuilder.com/columns/date-time-php/)