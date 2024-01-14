---
title:    "Javascript: Beregning av en dato i fremtiden eller fortiden"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger i programmering må vi håndtere datoer og tider. Kanskje vi trenger å beregne en dato i fremtiden eller fortiden for å planlegge noe, eller kanskje vi ønsker å datere et blogginnlegg for å publisere det senere. Uansett årsak, er det viktig å kunne håndtere dette problemet på en effektiv måte. I denne bloggposten vil vi se på hvordan man kan beregne en dato i fremtiden eller fortiden i Javascript.

## Slik gjør du

Det finnes flere måter å beregne en dato i Javascript på, men vi skal fokusere på den mest effektive metoden ved å bruke det innebygde Date-objektet og dets metoder. La oss si at vi ønsker å beregne en dato en uke fra nå. Dette kan gjøres ved å bruke setDate() metoden til å legge til 7 dager til den nåværende datoen.

```Javascript
let today = new Date();
today.setDate(today.getDate() + 7);
console.log(today); //output: Thu Apr 15 2021 19:59:09 GMT+0200 (Central European Summer Time)
```

På samme måte kan vi også beregne en dato i fortiden ved å bruke setDate() metoden til å trekke fra et antall dager fra den nåværende datoen.

```Javascript
let today = new Date();
today.setDate(today.getDate() - 10);
console.log(today); //output: Sun Mar 28 2021 19:59:09 GMT+0200 (Central European Summer Time)
```

Vi kan også bruke andre metoder som setMonth() og setFullYear() for å beregne datoer i fremtiden eller fortiden, avhengig av hva slags nøyaktighet vi trenger.

## Dypdykk

Når vi bruker Date-objektet, må vi være oppmerksomme på at datoer og tider er sensitive for tidsforskjeller og tidssoner. Dette betyr at hvis vi for eksempel ønsker å beregne en dato 6 måneder fra nå, så vil dette resultatet variere avhengig av hvilken tidssone vi befinner oss i. Derfor er det viktig å alltid være oppmerksom på begreper som UTC (Universal Time Coordinated) og UTC-offset når man jobber med tid og datoer.

En annen viktig ting å være klar over er at Javascripts Date-objektet har en begrensning på antall millisekunder det kan håndtere. Dette betyr at hvis vi prøver å beregne datoer for langt inn i fremtiden eller fortiden, vil dette gi ugyldige resultater. Derfor må vi være forsiktige og sørge for at vi ikke overskrider denne begrensningen når vi beregner datoer.

## Se også

- [Date Object Reference fra W3Schools](https://www.w3schools.com/jsref/jsref_obj_date.asp)
- [Kurs om tid og dato manipulasjon i Javascript fra Udemy](https://www.udemy.com/course/dates-data-time-manipulation-in-javascript/learn/lecture/2663980#overview)
- [Tidsforskjeller og UTC-offset for ulike tidssoner fra Timeanddate.com](https://www.timeanddate.com/time/map/)