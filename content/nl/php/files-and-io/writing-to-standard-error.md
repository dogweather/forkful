---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:39.772151-07:00
description: "Schrijven naar standaardfout (`stderr`) betekent het uitvoeren van foutmeldingen\
  \ en diagnostiek los van de standaarduitvoer (`stdout`). Programmeurs doen\u2026"
lastmod: '2024-03-11T00:14:24.743051-06:00'
model: gpt-4-0125-preview
summary: "Schrijven naar standaardfout (`stderr`) betekent het uitvoeren van foutmeldingen\
  \ en diagnostiek los van de standaarduitvoer (`stdout`). Programmeurs doen\u2026"
title: Schrijven naar standaardfout
---

{{< edit_this_page >}}

## Wat & Waarom?

Schrijven naar standaardfout (`stderr`) betekent het uitvoeren van foutmeldingen en diagnostiek los van de standaarduitvoer (`stdout`). Programmeurs doen dit om code te debuggen en foutinformatie te verschaffen zonder dit te vermengen met reguliere programma-uitvoer.

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
