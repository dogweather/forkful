---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:50.991805-07:00
description: "Hvordan: \xC5 opprette og bruke assosiative arrays (objekter) i JavaScript\
  \ er enkelt. Du definerer et objekt med kr\xF8llparenteser `{}`, og innenfor disse\
  \ kan\u2026"
lastmod: '2024-03-13T22:44:41.177100-06:00'
model: gpt-4-0125-preview
summary: "\xC5 opprette og bruke assosiative arrays (objekter) i JavaScript er enkelt."
title: Bruke associative tabeller
weight: 15
---

## Hvordan:
Å opprette og bruke assosiative arrays (objekter) i JavaScript er enkelt. Du definerer et objekt med krøllparenteser `{}`, og innenfor disse kan du definere et sett av nøkkel-verdipar. Nøkler er alltid strenger, og verdier kan være hva som helst: strenger, tall, arrays, til og med andre objekter.

```javascript
// Opprette et assosiativt array
let userInfo = {
  name: "Alex",
  age: 30,
  email: "alex@example.com"
};

// Få tilgang til elementer
console.log(userInfo.name); // Utdata: Alex
console.log(userInfo["email"]); // Utdata: alex@example.com

// Legge til nye elementer
userInfo.job = "Utvikler";
userInfo["country"] = "Canada";

console.log(userInfo);
/* Utdata:
{
  name: "Alex",
  age: 30,
  email: "alex@example.com",
  job: "Utvikler",
  country: "Canada"
}
*/

// Slette et element
delete userInfo.age;
console.log(userInfo);
/* Utdata:
{
  name: "Alex",
  email: "alex@example.com",
  job: "Utvikler",
  country: "Canada"
}
*/
```

Som du kan se, er tilgang, tillegg eller sletting av elementer i et assosiativt array ganske direkte og intuitivt.

## Dypdykk
I JavaScript-verdenen, selv om vi ofte hører uttrykket "assosiativt array," er det teknisk sett en feilbetegnelse fordi JavaScript ikke har ekte assosiative arrays som andre språk (for eksempel PHP). Det JavaScript har, er objekter som tjener et lignende formål, men som er en mer kraftfull og fleksibel konstruksjon.

Historisk sett var arrays i programmeringsspråk designet for å holde på en samling av gjenstander, tilgjengelig ved deres numeriske indeks. Imidlertid, som programvareutvikling utviklet seg, oppsto behovet for mer fleksible datastrukturer. Assosiative arrays, eller ordbøker i andre språk, var ett svar, som tillot tilgang til elementer gjennom vilkårlige nøkler.

JavaScripts tilnærming med objekter som nøkkel-verdi-lagre tilbyr en blanding av funksjonalitet. Det tillater egenskaper (nøkler) å bli lagt til, fjernet og oppslått ved navn. JSON (JavaScript Object Notation) er et vitnesbyrd om nytten av denne strukturen og har blitt den de facto standarden for datautveksling på nettet.

Selv om objekter dekker de fleste behov for assosiative arrays, i tilfeller der nøkkelrekkefølge eller iterering er viktig, gir `Map`-objektet introdusert i ES6 et bedre alternativ. En `Map` beholder nøkkelorden, aksepterer et bredere utvalg av datatyper som nøkler, og inkluderer nyttige metoder for iterering og størrelsesopphenting. Til tross for disse fordelene, forblir den tradisjonelle objektsyntaksen populær for sin enkelhet og brukervennlighet i mange vanlige scenarioer.
