---
date: 2024-01-20 17:33:22.109861-07:00
description: "Hvordan: F\xF8r PHP 5.2.0, sammenlignet programmerere ofte datoer ved\
  \ \xE5 konvertere dem til Unix-tidstamp med `strtotime()`. Alternativet er \xE5\
  \ bruke\u2026"
lastmod: '2024-04-05T21:53:41.865828-06:00'
model: gpt-4-1106-preview
summary: "F\xF8r PHP 5.2.0, sammenlignet programmerere ofte datoer ved \xE5 konvertere\
  \ dem til Unix-tidstamp med `strtotime()`."
title: Sammenlikning av to datoer
weight: 27
---

## Hvordan:
```PHP
<?php
$date1 = new DateTime("2023-03-30");
$date2 = new DateTime("2023-04-15");

if ($date1 > $date2) {
  echo "Date1 er senere enn Date2";
} elseif ($date1 < $date2) {
  echo "Date1 er tidligere enn Date2";
} else {
  echo "Datoene er like";
}

// Differanse
$diff = $date2->diff($date1);
echo "Forskjellen er " . $diff->days . " dager";

// Eksempeloutput:
// Date1 er tidligere enn Date2
// Forskjellen er 16 dager
?>
```

## Dypdykk:
Før PHP 5.2.0, sammenlignet programmerere ofte datoer ved å konvertere dem til Unix-tidstamp med `strtotime()`. Alternativet er å bruke `DateTime` klassen som gir større fleksibilitet og nøyaktighet, spesielt rundt skuddår og tidssoner. Når du implementerer dato-sammenligning, tenk på tidssonehåndtering og formatering slik at sammenligningen blir korrekt under forskjellige forhold.

## Se Også:
- [PHP manual on DateTime](https://www.php.net/manual/en/class.datetime.php)
- [PHP Date/Time functions](https://www.php.net/manual/en/ref.datetime.php)
- [DateInterval class manual](https://www.php.net/manual/en/class.dateinterval.php)
