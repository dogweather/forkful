---
title:                "Beregning av datoer i fremtiden eller fortiden"
html_title:           "Javascript: Beregning av datoer i fremtiden eller fortiden"
simple_title:         "Beregning av datoer i fremtiden eller fortiden"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?
Beregning av en dato i fremtiden eller fortiden handler om å bestemme en spesifikk dato basert på et gitt antall dager fremover eller bakover. Dette er en vanlig oppgave for programmører når de jobber med tidspunkt og datoer i applikasjoner og systemer. Ved hjelp av disse beregningene kan vi enkelt planlegge og håndtere hendelser og brukere.

## Hvordan:
For å beregne en dato i fremtiden eller fortiden i Javascript, kan du bruke Date-objektet og dets metoder. La oss si at vi vil finne ut hvilken dato som er 100 dager etter dagens dato. Da kan vi bruke følgende kode:

```Javascript
let today = new Date(); // Oppretter et Date-objekt for dagens dato
let futureDate = new Date(today.getTime() + 100 * 24 * 60 * 60 * 1000); // Legger til 100 dager i millisekunder på dagens dato
console.log(futureDate); // Skriver ut datoen for 100 dager etter dagens dato
```

Dette vil gi oss følgende output:

```
Sat Sep 25 2021 19:45:11 GMT+0200 (Central European Summer Time)
```

På samme måte kan vi beregne en dato i fortiden ved å trekke fra et gitt antall dager i millisekunder fra dagens dato.

## Dypdykk:
Beregning av datoer har vært en viktig del av programmeringen siden gamle dager, og det finnes mange forskjellige måter å håndtere det på avhengig av programmeringsspråket. I Javascript kan vi bruke Date-objektet, som er en del av ECMAScript-standarden. Dette gir oss en enkel og pålitelig måte å håndtere datoer på i våre applikasjoner. Andre språk kan ha sine egne innebygde funksjoner eller biblioteker for å håndtere datoer.

## Se også:
- Mer informasjon om Date-objektet i Javascript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date 
- En alternativ måte å håndtere datoer på i Javascript ved hjelp av biblioteket Moment.js: https://momentjs.com/