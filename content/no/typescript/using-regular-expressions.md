---
title:                "TypeScript: Å bruke regulære uttrykk"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Regular expressions er en kraftig teknikk som lar deg søke, filtrere og manipulere tekststrenger basert på et mønster. De er nyttige for å finne og endre data, og kan være en effektiv måte å håndtere store datamengder på. Ved å lære hvordan man bruker regular expressions kan du effektivisere din TypeScript programmering og få tilgang til avanserte tekstmanipulasjonsfunksjoner.

## Hvordan

Å bruke regular expressions i TypeScript er veldig likt som å bruke dem i andre programmeringsspråk. Du må først definere et mønster som representerer hva du vil søke etter, og deretter bruke dette mønsteret i en metode som skal søke gjennom en tekststreng. La oss se på et eksempel:

```TypeScript
//Først definerer vi mønsteret vi vil søke etter
let regex = /hello/;

//Deretter bruker vi det i en test-metode for å se om det finnes i en tekststreng
console.log(regex.test("Hello World")); // Output: true
console.log(regex.test("Bye bye")); // Output: false
```

Som du kan se er det første trinnet å definere et mønster ved å bruke skråstrek / foran og bak mønsteret. Dette forteller TypeScript at dette er et regular expression mønster. Deretter bruker vi metoden `.test()` for å søke gjennom teksten og returnere true eller false basert på om mønsteret finnes. Du kan også bruke `.exec()` metoden for å få informasjon om hvor i teksten mønsteret finnes.

## Deep Dive

Det finnes mange forskjellige metoder og spesialtegn som kan brukes i regular expressions for å gjøre søket mer avansert. Her er noen eksempler:

- `[]` - Definerer en gruppe av karakterer som du vil søke etter. For eksempel `[aeiou]` vil matche alle vokaler.
- `.` - Kan brukes for å matche alle karakterer bortsett fra linjeskift.
- `+` - Brukes for å matche en eller flere forekomster av det forrige uttrykket. For eksempel vil `a+` matche alle strenger som har en eller flere a-er.
- `\d` - Brukes for å matche et tall.
- `^` - Brukes for å matche begynnelsen av en tekststreng.
- `$` - Brukes for å matche slutten av en tekststreng.

For mer informasjon og en fullstendig liste over metoder og spesialtegn kan du sjekke ut denne ressursen: https://www.w3schools.com/jsref/jsref_obj_regexp.asp

## Se også

- [TypeScript offisiell dokumentasjon](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [W3Schools RegEx Tutorial](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
- [MDN RegEx Guide](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)