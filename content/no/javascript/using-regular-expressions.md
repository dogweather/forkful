---
title:                "Å bruke regulære uttrykk"
html_title:           "Javascript: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Regulære uttrykk, eller "regular expressions", er et kraftig verktøy for å søke etter og manipulere tekst i JavaScript. Det kan spare utviklere mye tid og forenkle arbeidsflyten deres når de jobber med tekstbaserte data.

## Hvordan

For å bruke regulære uttrykk i JavaScript, må du først definere et mønster som du vil søke etter i en tekststreng. Dette mønsteret kan inkludere forskjellige symboler og spesielle karakterer som vil hjelpe deg å finne ønsket data. Et eksempel på et mønster kunne være `/apple/`, som vil søke etter alle instanser av ordet "apple" i en tekst. 

For å bruke dette mønsteret i koden din, bruker vi `test()`-metoden eller `exec()`-metoden på et regulært uttrykkobjekt. La oss ta en titt på et eksempel:

```Javascript
let tekst = "Jeg elsker å spise epler";
let mønster = /epler/;
let resultat = mønster.test(tekst);

console.log(resultat); // Output: true
```

Vi kan også bruke spesielle symboler for å gjøre søket vårt mer spesifikt. For eksempel kan vi bruke "globale" (`g`) og "sensitivitetsnivå" (`i`) flagg for å søke etter flere forekomster av et mønster og ignorere store og små bokstaver. Her er et eksempel på dette:

```Javascript
let tekst = "Jeg elsker å spise epler og appelsiner";
let mønster = /Epler/; // Ignorerer store og små bokstaver 
let resultat = mønster.test(tekst);

console.log(resultat); // Output: true
```

Vi kan også bruke "meta-tegn" som `^` for å finne et mønster i starten av en tekst, `$` for å finne det i slutten av en tekst og `.` for å finne alle karakterer mellom to punkter. La oss se dette i aksjon:

```Javascript
let tekst = "I dag skal jeg på joggetur";
let mønster = /^jogge/; // Finner mønsteret "jogge" i starten av teksten
let resultat = mønster.test(tekst);

console.log(resultat); // Output: false
```

Vi kan også bruke regulære uttrykk til å erstatte tekst. Dette gjøres ved å bruke `replace()`-metoden på en tekststreng og spesifisere to parametere: den gamle teksten som skal erstattes og den nye teksten den skal erstattes med. Her er et eksempel:

```Javascript
let tekst = "Jeg elsker å spise epler og appelsiner";
let nyTekst = tekst.replace(/appelsiner/g, "sitroner"); // Erstatter "appelsiner" med "sitroner"
console.log(nyTekst); // Output: "Jeg elsker å spise epler og sitroner"
```

## Dypdykk

Regulære uttrykk kan også brukes til mer avanserte manipuleringer av tekst. For eksempel kan man bruke "gruppering" (`()`) og "alternativer" (`|`) for å søke etter flere mønstre samtidig. Man kan også bruke "kvantifisører" (`?`, `*`, `+`, `{}`) for å spesifisere hvor mange ganger et mønster skal forekomme i teksten. Det finnes også en rekke forskjellige "spesialtegn" som kan brukes til å finne og erstatte tekst i spesifikke mønstre.

For å lære mer om regulære uttrykk og alle mulighetene de gir, anbefaler vi å sjekke ut lenkene nedenfor.

## Se også

- [MDN Web Docs om regulære uttrykk i JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [RegExr - et interaktivt verktøy for å teste og lære om regulære uttrykk](https://regexr.com/)