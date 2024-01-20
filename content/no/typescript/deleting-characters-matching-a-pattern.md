---
title:                "Slette tegn som samsvarer med et mønster"
html_title:           "Arduino: Slette tegn som samsvarer med et mønster"
simple_title:         "Slette tegn som samsvarer med et mønster"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Slett karakterer som matcher et mønster i TypeScript

## Hva & Hvorfor?

Sletting av karakterer som matcher et mønster er en operasjon som fjerner alle forekomster av en bestemt tegnsekvens i en streng. Dette er viktig for å manipulere og formatere data effektivt.

## Hvordan:

Bruk `replace()`-funksjonen i TypeScript til å fjerne alle forekomster av et gitt mønster. For eksempel:

```TypeScript
var tekst: string = "Hei, hvordan har du det?";
console.log(tekst.replace(/h/gi, ''));
```

Output vil være: `ei, voran ar du det?`

Her erstatter `replace(/h/gi, '')` alle 'h'-er i strengen med '' (ingenting), altså sletter den.

## Dypdykk

Sletting av mønstermatching karakterer har sin opprinnelse i behovet for å rense og forberede data. Det gir muligheten til å fjerne unødvendige, overflødige eller feilplottede tegn.

Det er flere måter å utføre denne operasjonen på i tillegg til `replace()`-metoden. For eksempel, fremgangsmåter som bruker regulære uttrykk (RegExp).

Vær oppmerksom på at `replace()` i TypeScript ikke endrer den opprinnelige strengen, men returnerer en ny streng.

## Se også

- [TypeScript docs on Strings](https://www.typescripttutorial.net/typescript-tutorial/typescript-string/)
- [Wikipedia - Regular Expression](https://en.wikipedia.org/wiki/Regular_expression)