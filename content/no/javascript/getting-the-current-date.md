---
title:    "Javascript: Hente den nåværende datoen."
keywords: ["Javascript"]
---

{{< edit_this_page >}}

# Hvorfor

Når vi programmerer, er det ofte nødvendig å få tilgang til nåværende dato og klokkeslett. Dette kan være nyttig for å vise tidspunktet for en hendelse, beregne alder eller planlegge en fremtidig handling. Uansett årsak, er det viktig å vite hvordan man kan få tak i denne informasjonen i Javascript.

# Hvordan få tak i nåværende dato

For å få tak i nåværende dato i Javascript, kan vi bruke et innebygd objekt kalt "Date". Dette objektet har en rekke nyttige metoder og egenskaper for å håndtere datofunksjonalitet.

Vi kan først opprette en ny forekomst av Dato-objektet ved å kalle `new Date()`. Dette vil referere til nåværende dato og klokkeslett.

```Javascript
let nåværendeDato = new Date();
console.log(nåværendeDato);
// Output: 2021-03-27T14:12:03.210Z
```

Vi kan også spesifisere en bestemt dato og klokkeslett ved å gi argumenter til Date-konstruktøren. Dette gjøres på følgende måte: `new Date(år, måned, dag, time, minutt, sekund)`.

```Javascript
let dato = new Date(2021, 2, 27, 12, 30, 0);
console.log(dato);
// Output: 2021-02-27T11:30:00.000Z
```

Vi kan også få tilgang til ulike deler av datoen ved å bruke de ulike metodene på Dato-objektet. For eksempel kan vi få tak i måneden ved å bruke `dato.getMonth()`, som vil returnere verdien 2 siden måneder i Javascript starter på 0.

```Javascript
let måned = dato.getMonth();
console.log(måned);
// Output: 2
```

# Dykke dypere

Selv om dette bare er noen få eksempler på hvordan man kan få tak i nåværende dato i Javascript, har Date-objektet mye mer funksjonalitet som kan utforskes.

For eksempel kan det være nyttig å vite at verdien som blir returnert av `dato.getMonth()` er basert på din lokale tidssone og ikke på UTC-tidssonen som den vises som i konsollen.

Det er også verdt å merke seg at Dato-objektet ikke bare kan håndtere nåværende dato og klokkeslett, men også tidligere og fremtidige datoer og tider.

# Se også

- Date-objektet (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Tidssoner og Dato-objektet (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date#Time_zone)
- Generering av tilfeldige datoer i Javascript (https://www.freecodecamp.org/news/javascript-generate-random-dates/)