---
title:    "TypeScript: Skriver til standardfeil"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standardfeil er en viktig del av programmering for å kunne fange feil og problemer som oppstår under kjøring av et program. Denne praksisen hjelper deg med å finne og løse feil raskere, og gir deg bedre kontroll over koden din.

## Hvordan

For å skrive til standardfeil i TypeScript, kan du bruke funksjonen `console.error()`. Denne funksjonen tar inn en verdi eller variabel og skriver den til standardfeilstrømmen. For eksempel:

```TypeScript
let num1: number = 3;
let num2: number = 5;

if (num1 === num2) {
  console.error("Num1 og num2 er like");
} else {
  console.error("Num1 og num2 er ikke like");
}
```

Dette vil skrive følgende til standardfeil:

```
Num1 og num2 er ikke like
```

Du kan også skrive flere verdier til standardfeil ved å separere dem med komma, akkurat som med `console.log()`-funksjonen. Det kan være nyttig å skrive ut verdier av variabler og uttrykk for å sjekke at koden din fungerer som forventet.

## Dykk dypere

Når du skriver til standardfeil, er det viktig å merke seg at meldingene vil vises i terminalen eller konsollen, og ikke i selve nettleseren. Derfor er det viktig å sørge for at du kjører programmet ditt på riktig sted for å se feilmeldingene dine.

Du kan også kaste en `Error()` for å skrive en mer spesifikk feilmelding til standardfeil, og gi mer informasjon om hvor og hvordan feilen oppstod.

```
throw new Error("Ugyldig input: Variablen er ikke definert");
```

Husk at det er viktig å håndtere feil i koden din for å unngå krasj og uventet oppførsel. Ved å skrive til standardfeil, kan du gjøre feilsøkingsprosessen mye enklere og mer effektiv.

## Se også

- [TypeScript dokumentasjon om console.error()](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-6.html#console-error-assert)
- [Hvorfor og hvordan du håndterer feil i TypeScript](https://medium.com/@thebabscraig/handling-errors-in-typescript-3fcaba01ac4d)
- [Typer av feilhåndtering i TypeScript](https://blog.logrocket.com/error-handling-in-typescript-c8ab4c394003/)