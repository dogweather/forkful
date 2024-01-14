---
title:    "Javascript: Stor bokstaver i en streng"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Hvorfor

I programmering er det ofte viktig å ha kontroll over hvordan tekst vises for å gjøre den mer lesbar for brukeren. En måte å gjøre dette på er å kapitalisere strenger, det vil si å gjøre første bokstav i hvert ord til en stor bokstav. Dette kan være spesielt nyttig når man jobber med navn eller overskrifter.

## Slik gjør du det

For å kapitalisere en streng i Javascript, kan man bruke funksjonen toUpperCase() i kombinasjon med slice() og split() metoder. Her er et eksempel på hvordan dette kan gjøres:

```Javascript
let str = "dette er en tekst som skal kapitaliseres";

// Splitter strengen på mellomrom for å få et array med ord
let words = str.split(" ");

// Loop gjennom hvert ord
for(let i=0; i<words.length; i++){
    // Endre første bokstav til stor bokstav og resten av ordet til små bokstaver
    words[i] = words[i].charAt(0).toUpperCase() + words[i].slice(1).toLowerCase();
}

// Slår sammen arrayet til en streng igjen
let result = words.join(" ");
console.log(result);
// Output: Dette Er En Tekst Som Skal Kapitaliseres
```

Dette eksempelet viser hvordan man kan bruke tre forskjellige metoder sammen for å kapitalisere en streng. Ved å først splitte strengen på mellomrom, kan man behandle hvert ord individuelt og deretter sette dem sammen igjen til en streng som er kapitalisert.

## Dypdykk

Det finnes også andre måter å kapitalisere en streng på i Javascript, som for eksempel å bruke regular expressions eller funksjoner som toTitleCase(). Det viktigste er å finne en løsning som passer for din kode og sikrer at strengen blir riktig kapitalisert.

Det er også verdt å merke seg at denne metoden kun kapitaliserer første bokstav i hvert ord, så hvis du for eksempel har forkortelser som skal være i store bokstaver, må disse behandles separat.

## Se også

- [JavaScript String toUpperCase() metode](https://www.w3schools.com/jsref/jsref_touppercase.asp)
- [JavaScript String slice() metode](https://www.w3schools.com/jsref/jsref_slice_string.asp)
- [JavaScript String split() metode](https://www.w3schools.com/jsref/jsref_split.asp)