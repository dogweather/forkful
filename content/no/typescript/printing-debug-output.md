---
title:                "TypeScript: Utskrift av feilrettingsutdata"
programming_language: "TypeScript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Skaffe seg informasjon og forstå hva som skjer i koden din er viktig i enhver programmeringsprosess. Det er her debugging kommer inn, og utskrift av feil og verdier er en viktig del av denne prosessen. Utskrift av debug output kan hjelpe deg med å identifisere og løse problemer i koden din.

## Hvordan

For å skrive ut debug output i TypeScript, kan du bruke funksjonen `console.log()`. Denne funksjonen tar inn en eller flere verdier som parametere, og skriver dem ut i konsollen. La oss se på et eksempel:

```TypeScript
let navn: string = "Ole";
let alder: number = 25;
console.log("Hei, mitt navn er " + navn + " og jeg er " + alder + " år gammel.");
```

Dette vil skrive ut følgende i konsollen:

```
Hei, mitt navn er Ole og jeg er 25 år gammel.
```

Som du kan se, kan du bruke `+`-operatøren til å kombinere tekst og variabler i `console.log()`-funksjonen.

En annen måte å skrive ut verdier på er ved å bruke såkalte template literals, som er omsluttede med backticks (`). Disse lar deg sette inn variabler direkte i en tekststreng ved hjelp av `${}`-syntax.

```TypeScript
console.log(`Hei, mitt navn er ${name} og jeg er ${age} år gammel.`);
```

## Deep Dive

I tillegg til å skrive ut variabler og verdier, kan du også bruke `console.log()` for å se på ulike objekter eller matriser. For eksempel:

```TypeScript
let tallArray: number[] = [1, 2, 3, 4, 5];
console.log(tallArray);
```

Dette vil skrive ut hele matrisen `tallArray` til konsollen. Du kan også bruke `console.dir()` for å få en mer detaljert utskrift av objekter.

En annen nyttig funksjon er `console.assert()`, som tar inn en betingelse og en feilmelding. Hvis betingelsen ikke er sann, vil feilmeldingen bli skrevet ut i konsollen. Dette kan være nyttig for å sjekke at variabler og verdier holder forventet verdi.

## Se Også

- [TypeScript Dokumentasjon](https://www.typescriptlang.org/docs/)
- [Debugging Strategies for TypeScript](https://www.richardbultitude.com/blog/typescript-debugging#debuggingstrategiesfortypescript)
- [10 måter å forbedre ditt TS debugging-fell](https://medium.com/javascript-in-plain-english/10-ways-to-improve-your-typescript-debugging-game-f45315a5c2d9)