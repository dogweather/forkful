---
title:    "TypeScript: Å bruke regulære uttrykk"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Hvorfor bruke regulære uttrykk?

Regulære uttrykk er et kraftig verktøy for å søke og manipulere tekststrenger i programmering. Ved å bruke regulære uttrykk, kan du enkelt finne og erstatte deler av en tekst, validere brukerinput og mye mer. Det er en nyttig ferdighet å ha som utvikler, og kan spare deg for mye tid og frustrasjon. 

## Hvordan lage og bruke regulære uttrykk i TypeScript

For å lage regulære uttrykk i TypeScript, kan du bruke konstruktøren `RegExp`. Dette tar inn to argumenter: uttrykket du vil matche, og en valgfri flagg-parameter som spesifiserer hvordan matchingen skal utføres.

```TypeScript
let regex = new RegExp("hund", "i"); // Oppretter et nytt RegExp-objekt som matcher "hund" uavhengig av store og små bokstaver
```

Nå som du har en regulær uttrykk-instans, kan du bruke metoder som `test()` og `exec()` for å sjekke og manipulere tekststrenger.

```TypeScript
// Tester om "hund" finnes i strengen
regex.test("Jeg har en hund"); // Returnerer true
regex.test("Jeg har en katt"); // Returnerer false

// Eksporterer "hund" fra strengen og bytter ut den første forekomsten med "katt"
regex.exec("Jeg har en hund og en hund til"); // Returnerer "hund" og erstatter den første forekomsten med "katt"
```

Det finnes også mange forskjellige flagg som kan endre måten uttrykket ditt matcher på. For eksempel kan du bruke `g`-flagget for å matche flere forekomster i en tekst og `m`-flagget for å matche starten og slutten av hver linje i et flerlinjet uttrykk.

## Dypdykk i regulære uttrykk

Selv om det kan virke litt overveldende i starten, er det verdt å lære seg de ulike syntaksreglene og hvordan man bruker dem i regulære uttrykk. For eksempel kan du bruke spesielle tegn som `*` og `+` for å matche null eller flere eller én eller flere forekomster av en bestemt karakter eller gruppe av karakterer. Du kan også bruke `()` for å gruppere deler av uttrykket og bruke `|` for å matche flere muligheter.

Det finnes også mange nyttige verktøy og nettsteder som kan hjelpe deg å lære og teste regulære uttrykk, som [RegExr](https://regex101.com/) og [RegExr Playground for TypeScript](https://www.typescriptlang.org/play?#code/PTAEBcFg9gTgLgBOEYIYAMDcBhPUwC5UAzlGQDcDQgLAQwB2APS6IqATisAbhjISchgHI+tADgrbjw0CUISBACpJJgYIHoAoAEwFMAOqeJOyhygB7KIAvGTD8UbAEyzlZ0rLDoAbEAFkIACsixQkGwn4GKQUoLAAepW0rWLBYoHY8AKmiZ8gTjyCMpDoPMUnfHiyOoVgA) 

## Se også

- [Typescript Regex Expressions Tutorial](https://www.youtube.com/watch?v=qXN_F6T0qX4)
- [Introduction to Regular Expressions in JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Regular Expressions Cheatsheet](https://www.webfx.com/tools/regex-cheatsheet/)