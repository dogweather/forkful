---
date: 2024-01-20 17:31:37.721663-07:00
description: 'Hvordan: .'
lastmod: '2024-03-13T22:44:40.899872-06:00'
model: gpt-4-1106-preview
summary: .
title: Beregning av en dato i fremtiden eller fortiden
weight: 26
---

## Hvordan:
```PHP
<?php
// Dagens dato
$iDag = new DateTime();

// Legge til 10 dager
$iDag->modify('+10 days');
echo $iDag->format('Y-m-d') . "\n"; // viser fremtidig dato

// Trekke fra 10 dager
$iDag->modify('-10 days');
echo $iDag->format('Y-m-d') . "\n"; // viser fortidig dato
?>
```
Eksempel utdata:
```
2023-04-17
2023-03-28
```

## Deep Dive
Det å regne ut datoer har vært en del av programmering siden starten, da alt fra regneark til almanakker er avhengig av presis dato-håndtering. I PHP finnes alternative funksjoner som `strtotime()` og `date_add()`, men `DateTime` og `DateInterval` er mer robuste og objektorienterte. Disse klassene tar hensyn til skuddår, tidssoner og andre kompleksiteter ved datohåndtering. 

Implementeringsdetaljer:
- `DateTime`-klassen ble introdusert i PHP 5.2.0 og har siden blitt standard for dato-håndtering i PHP.
- `modify()`-metoden aksepterer en streng som forteller hvor mye tid som skal legges til eller trekkes fra.

## Se Også
- PHP.net sin dokumentasjon om `DateTime`: https://www.php.net/manual/en/class.datetime.php
- PHP.net sin dokumentasjon om `DateInterval`: https://www.php.net/manual/en/class.dateinterval.php
- PHP.net sin guide til forskjellige dato-funksjoner: https://www.php.net/manual/en/book.datetime.php
