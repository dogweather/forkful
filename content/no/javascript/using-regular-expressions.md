---
title:    "Javascript: Å bruke regulære uttrykk"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Regulære uttrykk, også kjent som regex, er en viktig verktøy i Javascript-programmering. De tillater oss å finne, manipulere og validere tekst på en enkel og effektiv måte. Hvis du ønsker å skrive mer robuste og dynamiske applikasjoner, er å lære hvordan man bruker regulære uttrykk en viktig ferdighet å ha.

## Hvordan

For å bruke regulære uttrykk i Javascript, må du først opprette et RegExp-objekt ved å bruke en av to metoder: `new RegExp(pattern, flags)` eller `/pattern/flags`. Patternet er mønsteret du ønsker å finne i en tekststreng, og flaggene kan være "i" for å ignorere store og små bokstaver, "g" for å finne alle den samme forekomsten i stedet for bare den første, og "m" for å aktivere flerlinje-søk.

Her er et eksempel på hvordan du kan bruke regulære uttrykk for å finne og erstatte ord i en tekststreng:

```Javascript
let tekst = "Dette er en tekststreng som jeg ønsker å endre.";
let regex = /tekststreng/;
let nyTekst = tekst.replace(regex, "ny tekst");
console.log(nyTekst); //Dette er en ny tekst som jeg ønsker å endre.
```

I dette eksempelet erstatter vi ordet "tekststreng" med "ny tekst". Du kan også bruke variabler og metode-kall i mønsteret for mer avansert søk og manipulering.

## Dypdykk

Regex er mye mer enn bare å finne og erstatte ord. Det gir deg muligheten til å søke etter bestemte mønstre som kan være uforutsigbare eller varierte. For eksempel kan du bruke "[]" for å finne alle karakterene som matcher et bestemt sett, "{}" for å angi repetisjoner, eller "\\b" for å matche ordgrenser.

Her er noen nyttige ressurser for å lære mer om hvordan du bruker regulære uttrykk i Javascript:

- [Eloquent Javascript](https://eloquentjavascript.net/09_regexp.html)
- [MDN Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Regex101](https://regex101.com/)

## Se også

- [Hvordan bruke funksjoner i Javascript](https://kodesnutt.com/hvordan-bruke-funksjoner-javascript)
- [Datastrukturer i Javascript](https://blogg.kodemaker.no/2014/12/datastrukturer-i-javascript/)