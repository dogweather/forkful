---
title:    "Javascript: Generering av tilfeldige tall"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

I dagens digitale verden er bruk av tilfeldige tall en viktig del av mange programmeringsoppgaver. Enten det er for å velge en tilfeldig vinner i en konkurranse, simulere et terningkast eller generere unike koder, så er tilfeldig tallgenerering en svært nyttig funksjon å kunne. Så hvorfor burde du lære deg hvordan man lager tilfeldige tall i Javascript? Vel, svaret er enkelt - det å kunne generere tilfeldige tall gjør deg til en mer allsidig og kompetent programmerer, og åpner opp for en rekke muligheter for å løse ulike problemer.

## Slik gjør du det

Heldigvis er det å lage tilfeldige tall i Javascript enkelt og intuitivt. Det første du trenger å gjøre er å inkludere Math biblioteket, som inneholder funksjoner for å utføre matematiske operasjoner. Deretter kan du bruke funksjonen Math.random() for å generere et tilfeldig tall mellom 0 og 1. For eksempel:

```Javascript
var tilfeldigTall = Math.random();
console.log(tilfeldigTall); // Output: 0.893265476
```

Dette vil generere et tilfeldig desimaltall hver gang koden kjøres. Men hva om du vil ha et tilfeldig tall innenfor et spesifikt område? For eksempel mellom 1 og 10? Da kan du bruke følgende formel:

```Javascript
var tilfeldigTall = Math.random() * (10 - 1) + 1;
console.log(tilfeldigTall); // Output: 7.648286529
```

Denne koden vil generere et tilfeldig tall mellom 1 og 10. Hvis du ønsker å få et heltall istedenfor et desimaltall, kan du bruke Math.floor() funksjonen til å avrunde ned. For eksempel:

```Javascript
var tilfeldigTall = Math.floor(Math.random() * (10 - 1) + 1);
console.log(tilfeldigTall); // Output: 7
```

Det finnes også andre måter å lage tilfeldige tall på i Javascript, som for eksempel å bruke arrays og loops. Utforsk mulighetene og prøv deg frem!

## Dypdykk

Å lage tilfeldige tall i programmering kan virke enkelt, men faktisk er det en rekke algoritmer og teknikker som kan brukes for mer komplekse behov. For eksempel kan du bruke en seed-verdi for å "kontrollere" tilfeldigheten og sikre at de samme tilfeldige tallene genereres hver gang koden kjøres. Du kan også bruke tilfeldige tall for å simulere forskjellige spill og gambling situasjoner.

Det er også viktig å merke seg at tilfeldige tall i Javascript ikke er helt tilfeldige, men heller "pseudo-tilfeldige" tall basert på en matematisk formel. I tillegg kan tilfeldige tallgenerering i forskjellige programmeringsspråk ha varierende nivåer av kompleksitet og nøyaktighet.

## Se også

- [Tilfeldige tall i Javascript](https://www.w3schools.com/js/js_random.asp)
- [Seed-verdi og tilfeldige tall](https://medium.com/@bongotilley/javascript-seeding-prngs-with-math-random-20de557e7619)
- [Kompleks tilfeldig tallgenerering i programmering](https://medium.com/datadriveninvestor/a-comprehensive-guide-to-random-number-generation-10d254bba8b2)