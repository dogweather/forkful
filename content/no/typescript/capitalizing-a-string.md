---
title:                "Sette stor forbokstav i en streng"
html_title:           "TypeScript: Sette stor forbokstav i en streng"
simple_title:         "Sette stor forbokstav i en streng"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Bogstavaktivering av Streng i TypeScript

## Hva & Hvorfor?

'Bogstavaktivering' av en streng betyr å endre første bokstav av hver ord til en stor bokstav. Programmerere gjør dette for bedre lesbarethet og organisering av data, spesielt i tekstbehandlingssystemer.

## Hvordan:

Her er et enkelt eksempel på hvordan du kan gjøre det i TypeScript:

```TypeScript
function capitalize(s: string): string {
  return s.split(' ')
    .map(word => word[0].toUpperCase() + word.substr(1))
    .join(' ');
}

console.log(capitalize("hei, hva skjer")); // Output: "Hei, Hva Skjer"
```

## Dypere dykk:

Historisk har 'bokstavaktivering' av strenger blitt brukt i mange kontekster, ikke bare programmering. Det er vanlig i typografi, der første bokstaven i et avsnitt ofte blir gjort større for å indikere begynnelsen av en ny tanke.

Det finnes flere alternative måter å gjøre denne handlingen på i forskjellige programmeringsspråk. I JavaScript kan det for eksempel gjøres med en kombinasjon av `charAt()` og `slice()` metoder:

```TypeScript
let string = 'hei, hva skjer';
let result = string.charAt(0).toUpperCase() + string.slice(1);
```

Vær oppmerksom på at 'bogstavaktivering' handler om teksttransformasjon, ikke datamanipulasjon. Det vil si, vi endrer måten dataene vises på, men ikke deres underliggende verdi.

## Se Også:

- String-metoder i JavaScript: https://developer.mozilla.org/no/docs/Web/JavaScript/Reference/Global_Objects/String
- En mer detaljert introduksjon til TypeScript: https://www.typescriptlang.org/docs/handbook/basic-types.html
- Tekstformatering og -transformasjon i programmering: [knytte](https://www.tutorialsteacher.com/articles/transform-string-in-programming)