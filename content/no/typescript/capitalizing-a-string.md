---
title:                "TypeScript: Store bokstaver i en tekststreng"
simple_title:         "Store bokstaver i en tekststreng"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere tekst til store bokstaver eller "kapitalisering" kan være viktig for å få en mer lesbar og konsistent kode. Dette kan forbedre lesbarheten og forståelsen av koden, spesielt for større prosjekter.

## Hvordan

```TypeScript
function capitalizeString(input: string): string {
  return input.toUpperCase(); // Eksempel på hvordan man enkelt kan konvertere en tekststreng til store bokstaver
}

console.log(capitalizeString("heisann")); // Output: HEISANN
``` 

En annen måte å kapitalisere en tekst på er ved å bruke metoden `replace` sammen med et regulært uttrykk, som gjør det mulig å kapitalisere bare den første bokstaven i en tekststreng:

```TypeScript
function capitalizeFirstLetter(input: string): string {
  return input.replace(/^\w/, (c) => c.toUpperCase()); // Finner første bokstav og kapitaliserer den
}

console.log(capitalizeFirstLetter("verden")); // Output: Verden
``` 

Hvis du har en tekststreng med flere ord, kan du også bruke metoden `split` for å splitte teksten ved mellomrom og deretter bruke `map` for å kapitalisere hvert ord:

```TypeScript
function capitalizeWords(input: string): string {
  return input.split(" ").map(w => w ? w[0].toUpperCase() + w.substr(1).toLowerCase() : w).join(" "); // Kapitaliserer hver enkelt ord
}

console.log(capitalizeWords("hei, hvordan går det?")); // Output: Hei, Hvordan Går Det?
``` 

## Dypdykk

I tillegg til å kapitalisere en hel tekststreng eller enkelte bokstaver, kan det også være nyttig å kunne ignorere og beholde spesifikke ord eller akronymer. Dette kan gjøres ved å bruke metoden `replace` sammen med et regex uttrykk som tar hensyn til disse ordene.

En annen ting å huske på er at i noen tilfeller kan ikke alle ord eller bokstaver kapitaliseres likt. For eksempel skal ikke forkortelser som "US" eller "UK" kapitaliseres som "Us" eller "Uk". Derfor kan du bruke en liste over slike unntak og sjekke om ordet er en del av den før du kapitaliserer det.

## Se også

- [JavaScript String Methods](https://www.w3schools.com/jsref/jsref_obj_string.asp)
- [Regular Expressions Tutorial](https://www.w3schools.com/js/js_regexp.asp)