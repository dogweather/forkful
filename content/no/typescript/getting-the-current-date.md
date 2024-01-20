---
title:                "Få den gjeldende datoen"
html_title:           "Haskell: Få den gjeldende datoen"
simple_title:         "Få den gjeldende datoen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å hente dagsdato er en metode for å få reda på nøyaktig det tidspunktet koden kjøres på. Dette kan være nyttig i mange sammenhenger, som å tidstemple en hendelse, bestemme tidsrom, og for lage dato-avhengige funksjoner.

## Hvordan:
Her er en enkel kode for å hente dagens dato i TypeScript.

```TypeScript
let iDag = new Date();
console.log(iDag);
```
Når du kjører denne koden vil du se noe likt dette:

```TypeScript
2022-05-20T10:20:42.618Z
```
Den returnerer dato og tid på ISO format (ÅÅÅÅ-MM-DDTHH:MM:SS.sssZ)

## Dypt Dykk
Henting av dagsdato har vært en funksjon i programmeringsspråk siden deres barndom. I tidlige programmeringsspråk var koden ofte mer kompleks, men moderne språk som TypeScript har innebygde funksjoner for dette.

Alternativt, kan vi også formatere dato-objektet til en mer lesbar form. For eksempel,

```TypeScript
let iDag = new Date();
let dato = iDag.getDate();
let maaned = iDag.getMonth() + 1; // Månedene starter på 0 i JavaScript/TypeScript
let aar = iDag.getFullYear();

console.log(aar + "-" + maaned + "-" + dato);
```
Ovenstående eksempel vil gi følgende resultat, som er enklere å lese for mennesker.

```TypeScript
2022-5-20
```

Husk at for å sikre maksimal kompatibilitet og presisjon, bruker TypeScript millisekunder som grunnleggende enhet for tid, selv om det gir mer informasjon enn hva som er nødvendig for de fleste anvendelser.

## Se også
* [MDN web docs: Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)