---
title:                "TypeScript: Sammenstilling av strenger"
simple_title:         "Sammenstilling av strenger"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du er en programutvikler som jobber med TypeScript, vet du sannsynligvis allerede at stringmanipulering er en viktig del av kodingen din. En av de mest grunnleggende og nyttige funksjonene innen stringmanipulering er konkatenering av strenger. Denne prosessen innebærer å kombinere to eller flere strenger for å danne en enkelt streng. Men hvorfor er dette viktig? Svaret er ganske enkelt: det gjør kodingen din mer fleksibel og lar deg håndtere variabel data på en effektiv måte.

## Hvordan du gjør det

For å konkatenere strenger i TypeScript, kan vi bruke "+" operatør til å kombinere strenger. La oss si at vi har to variabler, navn og yrke, som inneholder informasjonen "Johan" og "programmerer". For å kombinere disse to strengene, kan vi bruke følgende kode:

```TypeScript
let navn: string = "Johan;
let yrke: string = "programmerer";
let beskrivelse: string = navn + yrke;
console.log(beskrivelse);
```

Dette vil gi følgende utskrift i konsollen: "Johan programmerer". Som du kan se, blir de to strengene kombinert til en enkelt streng. Det er viktig å merke seg at rekkefølgen på strengene er viktig. Hvis vi bytter om rekkefølgen på "navn" og "yrke" i koden vår, vil utskriften være "programmerer Johan".

I tillegg til "+" operatøren, kan vi også bruke string interpolation for å konkatenere strenger i TypeScript. Dette gjøres ved å bruke " `${}`" syntaks. La oss se på et eksempel:

```TypeScript
let alder: number = 25;
let tekst: string = `Jeg er ${alder} år gammel.`
console.log(tekst);
```

Utskriften vil være "Jeg er 25 år gammel.". Merk at aldersvariabelen ble plassert inne i " `${}`" uttrykket mellom ordet "er" og setningen "år gammel".

## Dypdykk

Det er viktig å merke seg at når vi konkatenere tall og strenger i TypeScript, blir resultatet alltid en streng. Selv om alder variabelen i eksempelet vårt tidligere var en numerisk verdi, blir den omgjort til en streng når vi kombinerer den med en annen streng.

En annen ting å huske på er å bruke riktig data type når du arbeider med konkatenere. Hvis vi prøver å kombinere en streng og et tall uten å omgjøre tallet til en streng først, vil TypeScript gi oss en feilmelding.

## Se også

Hvis du ønsker mer informasjon om TypeScript, anbefaler vi å sjekke ut følgende ressurser:

- [Offisiell TypeScript dokumentasjon](https://www.typescriptlang.org/docs/home.html)
- [TypeScript på GitHub](https://github.com/microsoft/TypeScript)
- [Tutorial: Lag et TypeScript-prosjekt fra bunnen av](https://www.tutorialspoint.com/typescript/typescript_projects.htm)