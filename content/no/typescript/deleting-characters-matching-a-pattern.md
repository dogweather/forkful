---
title:    "TypeScript: Sletting av tegn som matcher et mønster"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Hvorfor

Å slette tegn som matcher et visst mønster er et vanlig oppgave i programmering. Det kan være for å opprettholde god kodepraksis, for å rydde opp i uønsket data eller for å løse et spesifikt problem. Uansett årsak, å kunne slette tegn basert på et mønster kan være nyttig og tidsbesparende.

## Slik gjør du det

For å slette tegn som matcher et mønster i TypeScript, kan vi bruke metoden `replace()` som finnes på alle strenger i språket. Denne metoden tar inn to argumenter - det første er mønsteret vi ønsker å finne og slette, og det andre er hva vi vil erstatte det med. La oss se på et eksempel:

```
TypeScript

let tekst = "Jeg elsker å spise pizza på fredager!";
let nyTekst = tekst.replace("elsker å ", "");

console.log(nyTekst); // Output: Jeg spiser pizza på fredager!
```

I dette eksempelet har vi brukt `replace()` til å fjerne "elsker å " fra teksten vår og erstattet det med ingenting. Dette resulterer i at teksten vår nå kun inkluderer det vi vil beholde.

Et annet vanlig scenario kan være å fjerne alle tall fra en tekststreng. I dette tilfellet kan vi bruke en såkalt regex (regulært uttrykk) for å identifisere alle tall og deretter bruke `replace()` for å slette dem. Her er et eksempel på hvordan det kan gjøres:

```
TypeScript

let tekst = "123Hei234 på 456deg 789!";
let regex = /[0-9]/g;
let nyTekst = tekst.replace(regex, "");

console.log(nyTekst); // Output: Hei på deg !
```

Her har vi brukt en regex som identifiserer alle tall mellom 0 og 9, og `g`-flagget angir at vi vil gjøre dette for alle forekomster i teksten. Deretter bruker vi `replace()` til å slette alle tallene og får til slutt en tekst uten tall.

## Dykk dypere

Nå som vi har sett hvordan vi kan slette tegn som matcher et mønster, la oss ta en titt på hvordan `replace()`-metoden faktisk fungerer. Denne metoden bruker et såkalt regular expression object (regex) til å finne og erstatte tegn. Dette objektet representerer et mønster som brukes for å søke etter en bestemt streng eller et mønster i en tekst.

I vårt eksempel brukte vi en regex som bare så etter tall, men det finnes utallige muligheter for hvordan man kan skrive en regex for å finne og slette bestemte tegn eller strenger i en tekst. Dette er et omfattende emne som krever en del kunnskap og erfaring, men det finnes gode ressurser på nettet for å lære mer om regex og hvordan man kan bruke det i sin kode.

## Se også

* [MDN Web Docs - String replace()](https://developer.mozilla.org/nb/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
* [Regex Tutorial - A Guide to Regular Expressions](https://www.regular-expressions.info/tutorial.html)
* [TypeScript Regular Expressions](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)