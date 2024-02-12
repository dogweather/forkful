---
title:                "Commandoregelargumenten lezen"
aliases:
- /nl/javascript/reading-command-line-arguments.md
date:                  2024-01-28T22:05:24.412933-07:00
model:                 gpt-4-0125-preview
simple_title:         "Commandoregelargumenten lezen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/javascript/reading-command-line-arguments.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Commandoregelargumenten lezen betekent het grijpen van de extra's die gebruikers toevoegen aan commando's wanneer ze je script uitvoeren. Programmeurs doen dit om gebruikers in staat te stellen gedrag aan te passen zonder code te wijzigen.

## Hoe:
Hier is de directe manier om het in Node.js te doen:

```javascript
// process.argv bevat commandoregelargumenten
const args = process.argv.slice(2);

console.log(args);

// Voer dit script uit met: node jouwscript.js eersteArgument tweedeArgument
```

Voorbeelduitvoer als je `node jouwscript.js ananas 42` uitvoert:

```javascript
['ananas', '42']
```

Een pakket zoals `yargs` gebruiken maakt het leven makkelijker, door je toe te staan argumenten bij naam te definiëren en te benaderen.

```javascript
// Installeer met npm install yargs
const yargs = require('yargs/yargs');
const { hideBin } = require('yargs/helpers');
const argv = yargs(hideBin(process.argv)).argv;

console.log(argv);

// Voer dit uit met: node jouwscript.js --fruit ananas --nummer 42
```

En je zou krijgen:

```javascript
{ fruit: 'ananas', number: '42' }
```

Schoon en duidelijk, met genoemde parameters.

## Diepe Duik
Lang geleden werden argumenten in C gelezen met `argc` en `argv` in de `main`-functie. In Node.js is `process.argv` de go-to. Het is een array waar het eerste element het pad is naar het node uitvoerbare bestand, het tweede is de scriptbestandsnaam, en de rest zijn je eigenlijke argumenten.

`yargs` is handig voor complexe apps: het analyseert argumenten in een handig object, regelt standaardwaarden, en genereert zelfs automatisch hulpberichten.

Er is ook het `minimist` pakket, een lichter alternatief voor `yargs`, als je van minimalisme houdt.

Diep van binnen gebruikt Node.js V8's `process.binding('options')` voor het parsen, wat niet is blootgesteld aan de gemiddelde gebruiker. Deze interne methode herbergt een hoop nut onder de kap, beheert het parsen en ophalen van commandoregelocties.

## Zie Ook
- Node.js process.argv documentatie: https://nodejs.org/docs/latest/api/process.html#process_process_argv
- Yargs GitHub repo: https://github.com/yargs/yargs
- Minimist op npm: https://www.npmjs.com/package/minimist
