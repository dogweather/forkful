---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:39.772151-07:00
description: 'Hoe te: Je kunt in PHP naar `stderr` schrijven met `fwrite()` of stream
  wrappers. Zo gaat het in zijn werk.'
lastmod: '2024-03-13T22:44:50.912268-06:00'
model: gpt-4-0125-preview
summary: Je kunt in PHP naar `stderr` schrijven met `fwrite()` of stream wrappers.
title: Schrijven naar standaardfout
weight: 25
---

## Hoe te:
Je kunt in PHP naar `stderr` schrijven met `fwrite()` of stream wrappers. Zo gaat het in zijn werk:

```PHP
<?php
// Schrijven naar stderr met fwrite
fwrite(STDERR, "Dit is een foutmelding.\n");

// Een stream wrapper gebruiken
file_put_contents('php://stderr', "Dit is nog een foutmelding.\n");
?>
```

Voorbeelduitvoer (in de console):
```
Dit is een foutmelding.
Dit is nog een foutmelding.
```

## Diepere Duik
Historisch gezien komt het scheiden van `stdout` en `stderr` van Unix's manier om I/O streams te behandelen. Andere talen zoals C hebben vergelijkbare conventies. Alternatieven in PHP kunnen het gebruik van logging bibliotheken of aangepaste foutafhandelaars inhouden, maar rechtstreeks naar `stderr` schrijven is eenvoudig voor console-applicaties. Achter de schermen is `stderr` een niet-gebufferde uitvoerstroom, wat betekent dat berichten onmiddellijk worden weggeschreven zonder te wachten.

## Zie Ook
- PHP Handleiding over Vooraf Gedefinieerde Constanten (STDERR): https://www.php.net/manual/en/features.commandline.io-streams.php
- PHP Handleiding over functies voor foutafhandeling: https://www.php.net/manual/en/book.errorfunc.php
- Wikipedia over Standaard streams: https://nl.wikipedia.org/wiki/Standaard_streams
