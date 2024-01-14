---
title:    "Javascript: Konvertering av streng til små bokstaver"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor
Å konvertere en streng til små bokstaver kan være nyttig hvis du for eksempel jobber med brukerinnganger eller må behandle tekst data på en bestemt måte.

## Hvordan du gjør det
```Javascript
// Første måte: ved hjelp av metoden toLowerCase()
let tekst = "Hei PÅ Deg";
let konvertertTekst = tekst.toLowerCase();

console.log(konvertertTekst);
// Output: hei på deg
```

```Javascript
// Andre måte: ved hjelp av for-løkke og ASCII-verdier
let tekst = "Hei PÅ Deg";
let konvertertTekst = "";
for (let i = 0; i < tekst.length; i++) {
    let tegn = tekst[i];
    // Sjekker om ASCII-verdien er innenfor A-Z området
    if (tegn.charCodeAt(0) >= 65 && tegn.charCodeAt(0) <= 90) {
        // Hvis ja, legges det til 32 for å få tilsvarende små bokstav ASCII-verdi
        konvertertTekst += String.fromCharCode(tegn.charCodeAt(0) + 32);
    } else {
        // Hvis ikke, legges det til uendret tegn i den konverterte teksten
        konvertertTekst += tegn;
    }
}

console.log(konvertertTekst);
// Output: hei på deg
```

## Deep Dive
Når vi snakker om å konvertere en streng til små bokstaver, mener vi å endre bokstavene fra deres opprinnelige store ASCII-verdi til sin tilsvarende små ASCII-verdi. Dette kan gjøres på forskjellige måter, men det viktigste å huske på er at det er viktig å ta hensyn til ulike språk og tegnsett. Dette kan påvirke resultatet av konverteringen, spesielt hvis man benytter seg av ASCII-verdier.

## Se også
- [Metoden toLowerCase() i Javascript](https://www.w3schools.com/JSREF/jsref_tolowercase.asp)
- [ASCII-tabell](https://ascii.cl/)
- [Utforsk ulike metoder for å jobbe med strenger i Javascript](https://www.freecodecamp.org/news/32-ways-to-use-javascripts-nobody-knows-about-d06c394ffc30/)