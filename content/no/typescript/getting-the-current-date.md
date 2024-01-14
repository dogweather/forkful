---
title:    "TypeScript: Å få gjeldende dato"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen gang har jobbet med datoer og tidsstempel i programmering, har du mest sannsynlig også måttet få tak i dagens dato på et eller annet tidspunkt. Dette kan være for å vise brukere når noe ble opprettet, for å lagre en tidssensitiv transaksjon, eller rett og slett for å gi informasjon om når koden ble kjørt. I denne bloggposten skal vi se på hvordan vi kan hente dagens dato ved hjelp av TypeScript.

## Hvordan

For å få tak i dagens dato i TypeScript, kan vi bruke JavaScripts innebygde Date-objekt. Dette objektet inneholder en rekke nyttige metoder og egenskaper for å håndtere datoer og tid. La oss se på et eksempel:

```TypeScript
const now = new Date();
console.log(now);
```

Dette vil gi følgende output i konsollen:

```bash
2021-07-20T13:54:25.446Z
```

Vi kan også formatere datoen etter våre egne preferanser ved hjelp av Date-objektet. For eksempel, hvis vi vil ha datoen i et mer leselig format, kan vi gjøre dette:

```TypeScript
const options = { year: 'numeric', month: 'long', day: 'numeric' };
const today = new Date().toLocaleDateString('no-NO', options);
console.log(today);
```

Dette vil gi følgende output:

```bash
20. juli 2021
```

Som du kan se, kan vi enkelt formatere datoen ved å angi hvilke deler av datoen vi vil ha med og hvilket språk vi ønsker å bruke.

## Dypdykk

Hvis vi ønsker å hente ut spesifikke deler av datoen, som for eksempel året eller måneden, kan vi bruke de innebygde egenskapene til Date-objektet. Dette kan være nyttig for å filtrere eller sortere data basert på dato. La oss se på et eksempel:

```TypeScript
const year = new Date().getFullYear();
console.log(year);
```

Dette vil gi følgende output:

```bash
2021
```

Vi kan også sammenligne datoer ved hjelp av innebygde metoder som `getTime()`. Dette kan være nyttig hvis vi for eksempel ønsker å sjekke om en dato er før eller etter en annen dato. Her er et eksempel på dette:

```TypeScript
const date1 = new Date('2021-01-01');
const date2 = new Date('2021-07-01');

if (date1.getTime() < date2.getTime()) {
  console.log('date1 er før date2');
} else {
  console.log('date2 er før date1');
}
```

Dette vil gi følgende output:

```bash
date1 er før date2
```

## Se også

- [Date-objektet i JavaScript](https://developer.mozilla.org/no/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Formatere datoer i JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat)