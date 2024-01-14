---
title:    "TypeScript: Beregning av dato i fremtiden eller fortiden."
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Hvorfor

Å beregne en dato i fremtiden eller fortiden kan være nyttig for å planlegge hendelser eller for å få en oversikt over når noe har skjedd. Dette kan gjøres ved hjelp av TypeScript-programmering, og i denne bloggposten vil vi lære hvordan.

## Slik gjør du det

For å beregne en dato i fremtiden eller fortiden, kan vi bruke TypeScript-funksjonen `Date()` og ulike metoder for å manipulere dato. Her er et eksempel på å beregne en dato 10 dager frem i tid:

```TypeScript
let today = new Date(); // oppretter en ny Dato-instans
today.setDate(today.getDate() + 10); // legger til 10 dager
console.log(today.toDateString()); // skriver ut resultatet: "Sun Mar 21 2021"
```

I dette eksempelet bruker vi `setDate()` metoden til å legge til 10 dager på dagens dato, og deretter bruker vi `toDateString()` for å få en lesbar dato.

For å beregne en dato i fortiden, kan vi bruke negativt tall. For eksempel, hvis vi vil ha datoen 5 dager tilbake i tid, kan vi bruke `-5` i `setDate()` metoden.

```TypeScript
let today = new Date(); 
today.setDate(today.getDate() - 5); 
console.log(today.toDateString()); // skriver ut resultatet: "Mon Mar 15 2021"
```

Dette er bare et enkelt eksempel, men det finnes flere metoder og funksjoner som kan brukes til å beregne datoer i fremtiden eller fortiden, som `setMonth()`, `setFullYear()`, `getDay()`, osv. Å utforske disse vil gi deg enda større fleksibilitet når du arbeider med datoer i TypeScript.

## Dykk dypere

For de som ønsker å forstå mer om hvordan disse funksjonene og metodene fungerer, kan vi se på noen av de grunnleggende konseptene.

For det første, når vi oppretter en ny `Date()` instans, får den dagens dato og klokkeslett som standardverdi. Datoobjekter håndteres internt som millisekunder siden 01.01.1970, som kalles "epoch time". Dette betyr at vi effektivt kan manipulere dato ved å legge til eller trekke fra et bestemt antall millisekunder.

Metodene som `setDate()` og `setMonth()` endrer kun den spesifikke delen av datoen, og gjør de nødvendige justeringene for å sikre at datoen fremdeles er gyldig. For eksempel, hvis vi bruker `setDate(31)` på en dato som allerede er 31., vil `setMonth()` også bli justert til å bli 1 måned frem i tid. 

Det er også verdt å merke seg at når vi bruker `getDate()` eller `getMonth()` for å hente verdier, returnerer disse verdier basert på lokal tidssone. Dette kan føre til forskjellige verdier for samme dato, avhengig av hvilken tidssone du er i.

## Se også

- [MDN: Date Object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript Date Object Documentation](https://www.typescriptlang.org/docs/handbook/standard-library.html#date)