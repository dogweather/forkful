---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:23.859910-07:00
description: "Strenginterpolering i Google Apps Script gj\xF8r det mulig for dynamisk\
  \ innlemming av uttrykk innenfor strenger, noe som letter skapelsen av mer lesbare\
  \ og\u2026"
lastmod: '2024-03-13T22:44:40.298849-06:00'
model: gpt-4-0125-preview
summary: "Strenginterpolering i Google Apps Script gj\xF8r det mulig for dynamisk\
  \ innlemming av uttrykk innenfor strenger, noe som letter skapelsen av mer lesbare\
  \ og vedlikeholdbare koder."
title: Interpolering av en streng
weight: 8
---

## Hva & Hvorfor?

Strenginterpolering i Google Apps Script gjør det mulig for dynamisk innlemming av uttrykk innenfor strenger, noe som letter skapelsen av mer lesbare og vedlikeholdbare koder. Programmerere bruker denne teknikken for sømløst å innarbeide variabler og uttrykk i strenger uten den tungvinte konkateneringssyntaksen.

## Hvordan:

I Google Apps Script oppnås strenginterpolering gjennom mal-literaler. Dette er strengliteraler som tillater innebygde uttrykk, angitt med omvendte anførselstegn (\`) i stedet for de vanlige anførselstegnene. Slik kan du bruke dem:

```javascript
// Et grunnleggende eksempel
function basicInterpolationExample() {
  const user = 'Alice';
  console.log(`Hei, ${user}!`); // Utdata: Hei, Alice!
}

// Bruk av uttrykk
function expressionInterpolationExample() {
  const a = 5;
  const b = 10;
  console.log(`Fem pluss ti er ${a + b}.`); // Utdata: Fem pluss ti er 15.
}

// Flere linje strenger
function multiLineStringExample() {
  const item = 'Google Apps Script';
  console.log(`Dette er en flerlinje streng:
Hallo alle sammen,
Vi diskuterer ${item} i dag.`);
  // Utdata:
  // Dette er en flerlinje streng:
  // Hallo alle sammen,
  // Vi diskuterer Google Apps Script i dag.
}

basicInterpolationExample();
expressionInterpolationExample();
multiLineStringExample();
```

Disse eksemplene illustrerer grunnleggende bruk, innlemming av uttrykk, og skapelsen av flerlinjestrenger med interpolerte verdier.

## Dypdykk

Mal-literaler, inkludert strenginterpolering, ble introdusert i ECMAScript 2015 (ES6) og deretter adoptert i Google Apps Script. Før dette måtte programmerere stole rent på strengkonkatenasjon, som kunne bli uhåndterlig for komplekse strenger eller når man integrerer mange variabelverdier.

```javascript
// Gamle måten (før ES6)
var user = 'Bob';
console.log('Hei, ' + user + '!');
```

Selv om strenginterpolering er en kraftig funksjon, er det viktig å være oppmerksom på kontekstene der den brukes. For eksempel kan direkte innlemming av brukerinput uten riktig sanitering føre til sikkerhetsproblemer, som injeksjonsangrep. Google Apps Script-utviklere bør sørge for at alt dynamisk innhold som er interpolert i strenger, er skikkelig sjekket eller sanert.

Sammenlignet med andre programmeringsspråk, finnes konseptet med strenginterpolering bredt, med varierende syntaks. Python bruker f-strenger eller `format`-metoden, Ruby bruker `#{}` innenfor dobbeltanførselte strenger, og mange moderne språk har adoptert lignende funksjoner på grunn av lesbarheten og bekvemmeligheten de tilbyr.

Selv om Google Apps Script ikke tilbyr ytterligere interpolyseringsfunksjoner utover de som tilbys av ECMAScript-standarder, er funksjonaliteten tilstede kraftfull og tilstrekkelig for de fleste bruksområder. Utviklere som kommer fra språk med mer utførte interpolyseringsmekanismer, kan trenge å justere sine forventninger, men vil sannsynligvis sette pris på enkelheten og effektiviteten av mal-literaler i Google Apps Script.
