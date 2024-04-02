---
date: 2024-01-20 17:42:27.276150-07:00
description: "I JavaScript kan vi slette tegn som matcher et m\xF8nster gjennom regul\xE6\
  re uttrykk for \xE5 rense strenger, trekke ut data eller for en rekke andre oppgaver\
  \ som\u2026"
lastmod: '2024-03-13T22:44:41.168460-06:00'
model: gpt-4-1106-preview
summary: "I JavaScript kan vi slette tegn som matcher et m\xF8nster gjennom regul\xE6\
  re uttrykk for \xE5 rense strenger, trekke ut data eller for en rekke andre oppgaver\
  \ som\u2026"
title: "Slette tegn som matcher et m\xF8nster"
weight: 5
---

## Hva & Hvorfor?
I JavaScript kan vi slette tegn som matcher et mønster gjennom regulære uttrykk for å rense strenger, trekke ut data eller for en rekke andre oppgaver som krever tekstmanipulasjon.

## Hvordan gjøre:
```javascript
// Eksempel: Fjerne alle tall fra en tekststreng
let tekst = 'Oslo1 er 2fantastisk 34 i 2023!';
let rensketTekst = tekst.replace(/\d+/g, '');
console.log(rensketTekst);  // 'Oslo er fantastisk  i !'

// Eksempel: Fjerne spesifikke tegnsett, som vokaler
let setning = 'Hvordan går det med deg?';
let utenVokaler = setning.replace(/[aeiouæøå]/gi, '');
console.log(utenVokaler);  // 'Hvrdn går dt md dg?'
```
Disse eksemplene bruker `String.prototype.replace()` metoden sammen med regulære uttrykk (`/\d+/g` og `/[aeiouæøå]/gi`) for å slette tegn etter mønster.

## Dypdykk
Historisk sett ble manipulering av strenger i JavaScript utfordt med enkle metoder som `indexOf()` og `slice()`. Dette hadde sine begrensninger. Introduksjonen av regulære uttrykk ga utviklere et kraftfullt verktøy for komplekse tekstmanipulasjonjobber.

Alternativt kan teknikker som splitting (`split()`) og gjenforening (`join()`) benyttes for enklere mønstre:

```javascript
// Alternativ metode for å fjerne alle tall
let tekst2 = '2023 blir et bra år!';
let tekstUtenTall = tekst2.split('').filter(c => isNaN(c) || c === ' ').join('');
console.log(tekstUtenTall);  // ' blir et bra år!'
```
Her splitter vi strengen til enkeltegn, filtrerer ut tall, og setter strengen sammen igjen.

En implementeringsdetalj: regulære uttrykk i JavaScript følger ECMAScript-standarder. `g`-flagget står for global søk og `i`-flagget for ignorering av store/små bokstaver. Disse flaggene bestemmer hvordan mønstersøket atferd seg.

## Se også
- JavaScript Regular Expressions: [MDN Web Docs - RegExp](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- String Manipulation in JavaScript: [MDN Web Docs - String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- Interactive regex tester: [RegExr](https://regexr.com/)
