---
title:                "Javascript: Konvertering av streng til små bokstaver"
simple_title:         "Konvertering av streng til små bokstaver"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Hvorfor

For mange programmerere kan det å konvertere en streng til små bokstaver virke som en enkel oppgave. Men det er faktisk en nyttig funksjon med mange bruksområder. Ved å konvertere en streng til små bokstaver, kan vi enkelt sammenligne og filtrere data, lage brukervennlige grensesnitt og mye mer.

# Hvordan

```Javascript
// Eksempel på konvertering av streng til små bokstaver
let navn = "Ola Nordmann";
let nyttNavn = navn.toLowerCase();
console.log(nyttNavn);
// Output: "ola nordmann"
```

Å konvertere en streng til små bokstaver i JavaScript er ganske enkelt. Vi bruker metoden `toLowerCase()` som kan brukes på en hvilken som helst streng. Den returnerer en ny streng med alle bokstaver i små bokstaver.

Dette kan også gjøres ved hjelp av for-løkker og vilkårssjekker, men det er mye enklere og mer effektivt å bruke den innebygde `toLowerCase()`-metoden.

```Javascript
// Konvertering av hvert tegn i en streng til små bokstaver
let navn = "Ola Nordmann";
let nyttNavn = "";
for (let i = 0; i < navn.length; i++) {
  if (navn[i].charCodeAt() < 91 && navn[i].charCodeAt() > 64) {
    // konverterer store bokstaver til små bokstaver ved å legge til 32 til Unicode-verdien
    nyttNavn += String.fromCharCode(navn[i].charCodeAt() + 32);
  } else {
    nyttNavn += navn[i];
  }
}
console.log(nyttNavn);
// Output: "ola nordmann"
```

Vi kan også bruke `toUpperCase()`-metoden for å konvertere en streng til store bokstaver på samme måte.

# Dypdykk

Det kan være nyttig å forstå hvordan datamaskinen håndterer bokstaver og tekst. I programmeringsspråk som JavaScript, brukes Unicode for å representere bokstaver og tegn. Dette er en standard som tildeler et unikt nummer til hver tegn i nesten alle språk og skriftsystemer i verden.

Når vi konverterer en streng til små bokstaver, bruker JavaScript denne Unicode-standarden til å finne de riktige små bokstavene som tilsvarer de store bokstavene i strengen. Dette gjør den innebygde metoden `toLowerCase()` både rask og pålitelig.

# Se også

- [JavaScript String toLowerCase() Method](https://www.w3schools.com/jsref/jsref_tolowercase.asp)
- [Unicode](https://home.unicode.org/)
- [ASCII og Unicode - en kort oversikt](https://www.mn.uio.no/ifi/tjenester/ikt/hjelp/programvare/forts/ascii-unicode.html)