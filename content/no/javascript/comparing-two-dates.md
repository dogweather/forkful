---
title:    "Javascript: Sammenligning av to datoer"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Sammenligning av datoer er en viktig del av programmering da det tillater oss å utføre forskjellige logiske operasjoner basert på dateringer. Det kan være nyttig for å sortere data, filtrere informasjon eller sjekke om en hendelse har skjedd før eller etter en annen hendelse.

## Hvordan

For å sammenligne to datoer i Javascript, kan vi bruke den innebygde `Date`-klassen og dens metoder. La oss se på et eksempel på hvordan vi kan sammenligne to datoer:

```javascript
let date1 = new Date("2021-01-01");
let date2 = new Date("2021-05-01");

if(date1 > date2) {
    console.log("date1 er senere enn date2");
} else if(date1 < date2) {
    console.log("date1 er før date2");
} else {
    console.log("date1 og date2 er lik");
}
```

I dette eksemplet bruker vi den logiske operatøren `>` og `<` for å sammenligne to datoer. Vi kan også bruke `==` for å sjekke om datoene er like. Output av dette eksemplet vil være "date1 er før date2", da 1. januar kommer før 1. mai.

Vi kan også sammenligne datoer med tidsstempel ved å bruke `getTime()`-metoden. Dette vil gi oss antall millisekunder siden 1. januar 1970, som er et vanlig referansepunkt for datoer i programmering. La oss se på et eksempel på dette:

```javascript
let date3 = new Date("2021-01-01");
let date4 = new Date("2021-01-02");

console.log(date3.getTime()); // 1609459200000
console.log(date4.getTime()); // 1609545600000
```

Her ser vi at datoene er forskjellige med en dag, men ved å bruke tidsstempel kan vi tydelig se at de er forskjellige med nøyaktig 86400000 millisekunder (1 dag).

## Dypdykk

Når vi sammenligner datoer i Javascript, er det viktig å vite at det er forskjellige måter å representere datoer på. Vi kan for eksempel bruke en tekststreng, `Date`-objekt eller et tidsstempel. Det er også viktig å være klar over at Javascript inneholder datoen i lokal tidssone, og at dette kan føre til noen uventede resultater når du sammenligner datoer fra forskjellige tidssoner.

For å håndtere dette, kan vi bruke `getTimezoneOffset()`-metoden for å få lokal tidssone og deretter justere datoene til å være i samme tidssone før vi sammenligner dem.

## Se også

- [Javascript Date Object](https://www.w3schools.com/jsref/jsref_date.asp)
- [MDN Web Docs - Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)