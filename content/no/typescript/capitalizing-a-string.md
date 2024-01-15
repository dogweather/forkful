---
title:                "Stor bokstaving av en streng"
html_title:           "TypeScript: Stor bokstaving av en streng"
simple_title:         "Stor bokstaving av en streng"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å bruke TypeScript for å kapitalisere en streng kan gjøre koden din mer lesbar og enklere å vedlikeholde. Det kan også være nyttig når du trenger å formatere data som skal vises for brukerne.

## Slik gjør du det

```TypeScript
const unformattedString = "dette er en tekst med små bokstaver";
const formattedString = unformattedString.toUpperCase();

console.log(formattedString);

// Output: DETTE ER EN TEKST MED SMÅ BOKSTAVER
```

I dette eksempelet har vi en tekst med små bokstaver og ved å bruke `.toUpperCase()`-metoden i TypeScript, vil teksten bli konvertert til store bokstaver. Dette er en enkel og effektiv måte å kapitalisere en streng på.

En annen måte å kapitalisere en streng på i TypeScript er å bruke en `for`-løkke og `'charAt()'`-metoden til å endre bokstaven på hvert index i strengen til en stor bokstav.

```TypeScript
let unformattedString = "dette er en tekst med små bokstaver";
let formattedString = '';

for(let i = 0; i < unformattedString.length; i++) {
  formattedString += unformattedString.charAt(i).toUpperCase();
}

console.log(formattedString);

// Output: DETTE ER EN TEKST MED SMÅ BOKSTAVER
```

## Dykk dypere

Når du bruker `.toUpperCase()` i TypeScript, blir alle bokstavene i strengen konvertert til store bokstaver i henhold til Unicode-standarden. Dette betyr at hvis du har en tekst på et annet språk enn engelsk, vil bokstavene bli konvertert til de store bokstavene i det respektive språket.

En annen ting å merke seg er at `.toUpperCase()`-metoden ikke endrer den opprinnelige strengen, den returnerer en ny streng med de kapitaliserte bokstavene.

## Se også

- [TypeScript Offisiell Dokumentasjon](https://www.typescriptlang.org/docs/home.html)
- [Unicode Character Table](https://unicode-table.com/en/)