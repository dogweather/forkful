---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:21.568948-07:00
description: "Schrijven naar standaard fout (`stderr`) stuurt foutmeldingen en diagnostische\
  \ berichten apart van standaard uitvoer (`stdout`). Programmeurs doen dit om\u2026"
lastmod: 2024-02-19 22:05:09.615257
model: gpt-4-0125-preview
summary: "Schrijven naar standaard fout (`stderr`) stuurt foutmeldingen en diagnostische\
  \ berichten apart van standaard uitvoer (`stdout`). Programmeurs doen dit om\u2026"
title: Schrijven naar standaardfout
---

{{< edit_this_page >}}

## Wat & Waarom?

Schrijven naar standaard fout (`stderr`) stuurt foutmeldingen en diagnostische berichten apart van standaard uitvoer (`stdout`). Programmeurs doen dit om te debuggen en fouten te loggen zonder de reguliere programma-uitvoer te vervuilen.

## Hoe:

In TypeScript kun je naar `stderr` schrijven met behulp van `console.error` of `process.stderr.write`. Hier zijn beide in actie:

```TypeScript
console.error("Dit is een foutmelding naar stderr");

process.stderr.write("Dit is nog een foutmelding naar stderr\n");
```

Voorbeelduitvoer voor beide regels:

```
Dit is een foutmelding naar stderr
Dit is nog een foutmelding naar stderr
```

## Diepere Duik

Historisch gezien liet het scheiden van `stdout` en `stderr` Unix-gebruikers toe om uitvoer en fouten naar verschillende bestemmingen te sturen. Je kon fouten loggen voor analyse terwijl je schone uitvoerdata had. Alternatieven voor het rechtstreeks schrijven naar `stderr` zijn onder meer logboekbibliotheken of -frameworks die meer controle en functies bieden. Implementatietechnisch gezien, omhult `console.error` `process.stderr.write` met extra formatteringsmogelijkheden, dus het gebruik van `console.error` is over het algemeen handiger voor eenvoudige berichten.

## Zie Ook

- Node.js documentatie over console: https://nodejs.org/api/console.html
- Node.js proces standaard streams: https://nodejs.org/api/process.html#process_process_stderr
- Discussie over `console.error` vs `process.stderr.write`: https://stackoverflow.com/questions/4976466/difference-between-process-stdout-write-and-console-log-in-node-js
