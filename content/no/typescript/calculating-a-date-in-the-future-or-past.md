---
title:    "TypeScript: Beregning av datoer i fremtiden eller fortiden"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Å beregne en dato i fremtiden eller fortiden kan være nyttig for å planlegge tidsfrister eller for å vise historiske datoer. Det kan også være en morsom og lærerik utfordring for utviklere å programmere.

## Hvordan

For å beregne en dato i fremtiden eller fortiden, kan vi bruke TypeScript sin Date-klasse og dens metoder. La oss si at vi vil beregne datoen 10 år fra i dag:

```TypeScript
let today = new Date(); // Oppretter et Date-objekt for dagens dato
let futureDate = new Date();
futureDate.setDate(today.getDate() + 365 * 10); // Setter datoen for 10 år fra i dag
```

Resultatet vil være en dato 10 år fra i dag. Hvis vi vil beregne en dato i fortiden, kan vi bruke samme metode og trekke fra ønsket antall dager i stedet.

Vi kan også formatere datoen ved å bruke metoder som `getMonth()`, `getFullYear()`, `getDay()`, etc. For eksempel, hvis vi vil ha datoen i formatet DD.MM.YYYY, kan vi gjøre følgende:

```TypeScript
let day = futureDate.getDate();
let month = futureDate.getMonth() + 1; // Månedene i Date-klassen starter på 0 (januar = 0)
let year = futureDate.getFullYear();

console.log(`${day}.${month}.${year}`); // Dette vil logge "14.04.2031" til konsollen
```

## Dypdykk

I tillegg til å beregne datoen i fremtiden eller fortiden, kan vi også bruke Date-klassen til å gjøre mer avanserte beregninger. For eksempel kan vi sjekke om en dato er en helligdag eller en helgedag:

```TypeScript
let holiday = new Date('12/25/2021'); // Oppretter et Date-objekt for julaften
console.log(holiday.getDay() === 0 || holiday.getDay() === 6); // Dette vil logge "true" siden 25. desember er en søndag
```

Vi kan også bruke metoden `getTime()` til å få tiden i millisekunder siden 1. januar 1970. Dette kan være nyttig når man sammenligner to datoer eller beregner tidsintervaller.

## Se også

- [Date-klasse dokumentasjon i TypeScript](https://www.typescriptlang.org/docs/handbook/2/objects.html#the-date-type)
- [Liste over norske helligdager](https://www.timeanddate.no/helligdager/norge/2021)
- [Blogginnlegg om å bruke TypeScript sitt Date-objekt](https://blog.angularindepth.com/date-handling-in-typescript-4a92152dbd7d)