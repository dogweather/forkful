---
title:                "Javascript: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Det finnes mange situasjoner hvor man trenger å kunne beregne en dato i fremtiden eller fortiden i et Javascript-program. Dette kan være nyttig for å planlegge fremtidige hendelser eller for å håndtere datoer i en database. I denne bloggposten vil jeg vise deg hvordan du kan gjøre dette på en enkel måte ved hjelp av Javascript.

## Hvordan

For å beregne en dato i fremtiden eller fortiden, trenger vi å bruke Date-objektet i Javascript. Dette objektet inneholder all informasjon om en dato og gjør det enkelt å manipulere den.

La oss si at vi ønsker å beregne en dato 3 dager frem i tid. Vi kan gjøre dette ved å først opprette et nytt Date-objekt og deretter bruke metoden setDate() til å legge til 3 dager til datoen. Deretter kan vi bruke metoden toLocaleDateString() for å formatere dato-objektet til en lesbar streng.

```javascript
let today = new Date();
today.setDate(today.getDate() + 3);
let futureDate = today.toLocaleDateString();
console.log(futureDate); // Output: 17. mai 2021
```

På samme måte kan vi også beregne en dato i fortiden ved å bruke metoden setDate() og angi et negativt tall. Her er et eksempel på hvordan vi kan beregne en dato for 1 uke siden:

```javascript
let today = new Date();
today.setDate(today.getDate() - 7);
let pastDate = today.toLocaleDateString();
console.log(pastDate); // Output: 3. mai 2021
```

## Dypdykk

Som nevnt tidligere inneholder Date-objektet mye nyttig informasjon om datoer. For å utforske dette mer grundig, kan vi bruke forskjellige metoder som er tilgjengelige for dette objektet.

En av disse metodene er getDay(), som returnerer nummeret som representerer dagen i uken (0-6). Dette kan være nyttig for å sjekke om for eksempel en dato er en helgedag eller ikke.

```javascript
let date = new Date();
let dayOfWeek = date.getDay();
console.log(dayOfWeek); // Output: 1 (mandag)
```

En annen nyttig metode er getMonth(), som returnerer nummeret som representerer måneden (0-11). Dette kan være spesielt viktig når man jobber med internasjonale datoformater som bruker forskjellige månedsnummereringer.

```javascript
let date = new Date();
let month = date.getMonth();
console.log(month); // Output: 4 (mai)
```

## Se også

- [Date-objektet i Javascript](https://developer.mozilla.org/nb/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Manipulering av datoer i Javascript](https://www.freecodecamp.org/news/how-to-manipulate-dates-in-javascript/)
- [Håndtering av datoer i et front-end rammeverk](https://blog.logrocket.com/working-with-dates-in-a-front-end-framework/)