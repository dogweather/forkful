---
title:                "Tolke en dato fra en streng"
date:                  2024-01-20T15:37:42.961961-07:00
html_title:           "Arduino: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Å tolke en dato fra en streng innebærer å konvertere tekst til et datoobjekt. Programmerere gjør dette for å manipulere og sammenligne datoer, og for å hente ut spesifikke datoelementer som dag, måned og år.

## How to:
I PHP bruker vi ofte `DateTime` klassen for å tolke datoer. Koden under viser hvordan.

```php
<?php
$datostreng = "2023-04-15 14:00:00";
$datoomforming = new DateTime($datostreng);
echo $datoomforming->format('Y-m-d H:i:s'); // Output: 2023-04-15 14:00:00
?>
```

Du kan også bruke `strtotime` for å konvertere en streng:

```php
<?php
$datostreng = "next Thursday";
$timestamp = strtotime($datostreng);
echo date('Y-m-d', $timestamp); // Output: (datoen for neste torsdag)
?>
```

## Deep Dive:
Å tolke datoer fra strenger var mer kronglete før `DateTime`-klassen ble introdusert i PHP 5.2. Før det stolte vi på `strtotime` og `date` funksjonene, som noen ganger kunne være inkonsekvente med ulike datoformater.

Alternativer inkluderer `intl`-utvidelsen for internasjonale datoformater, og `Carbon`, et tredjepartsbibliotek som gir mer omfattende funksjonalitet.

Når du tolker datoer, er det viktig å håndtere tidssoner korrekt. `DateTime`-klassen tillater spesifisering av tidssone ved opprettelse:

```php
<?php
$datostreng = "2023-04-15 14:00:00";
$tidsone = new DateTimeZone('Europe/Oslo');
$datoomforming = new DateTime($datostreng, $tidsone);
echo $datoomforming->format('Y-m-d H:i:sP'); // Output: 2023-04-15 14:00:00+02:00
?>
```

## See Also:
- [PHP Manual - DateTime](https://www.php.net/manual/en/class.datetime.php)
- [PHP Manual - strtotime](https://www.php.net/manual/en/function.strtotime.php)
- [Carbon - A simple PHP API extension for DateTime](https://carbon.nesbot.com/)
- [PHP Manual - IntlDateFormatter](https://www.php.net/manual/en/class.intldateformatter.php)
