---
date: 2024-01-26 03:36:52.734240-07:00
description: "Refaktorering er prosessen med \xE5 restrukturere eksisterende datamaskinkode\
  \ uten \xE5 endre dens eksterne oppf\xF8rsel. Programmerere gj\xF8r dette for \xE5\
  \ gj\xF8re\u2026"
lastmod: '2024-03-13T22:44:40.540566-06:00'
model: gpt-4-0125-preview
summary: "Refaktorering er prosessen med \xE5 restrukturere eksisterende datamaskinkode\
  \ uten \xE5 endre dens eksterne oppf\xF8rsel."
title: Refaktorering
weight: 19
---

## Hva & Hvorfor?
Refaktorering er prosessen med å restrukturere eksisterende datamaskinkode uten å endre dens eksterne oppførsel. Programmerere gjør dette for å gjøre koden renere, lettere å vedlikeholde, og for å redusere kompleksiteten, noe som gjør den lettere å forstå for noen som dykker inn i den frisk.

## Hvordan:
Tenk på en TypeScript-funksjon som har sett bedre dager - den er litt rotete, og kunne trenge litt kjærlig omsorg:

```typescript
function userInfo(data: any): string {
    return "User Info: " + data.name + ", " + data.age + ", " + data.email + ";" ;
}
```
Refaktorert, kan dette se slik ut:

```typescript
interface User {
    name: string;
    age: number;
    email: string;
}

function formatUserInfo(user: User): string {
    return `User Info: ${user.name}, ${user.age}, ${user.email};`;
}
```

Det andre eksemplet er mer robust, og utnytter TypeScript sitt typesystem med et `interface` for å unngå potensielle kjøretidsfeil og forbedre lesbarheten.

## Dypdykk
Refaktorering er ikke et moderne konsept; det har utviklet seg med programmering og ble mer formalisert med utgivelsen av Martin Fowlers bok "Refaktorering: Forbedring av designet på eksisterende kode" i 1999. Det er avgjørende i et Agile utviklingsmiljø, og legger til rette for adaptive kodeendringer. Noen alternativer til manuell refaktorering inkluderer automatiserte verktøy som TSLint eller TypeScript sin egen språkserver som kan foreslå eller til og med utføre visse refaktoreringsoppgaver for deg. Gjennomføringsdetaljer involverer vanligvis å gjenkjenne "kode dårlig lukt", som duplisert kode, lange metoder eller store klasser, og anvende mønstre for å avhjelpe - som å trekke ut metoder, flytte til mer passende klasser, eller bruke enklere konstruksjoner. Disse mønstrene er nøkkelen til å forstå hvordan og hvorfor av refaktorering.

## Se Også
- [Boken "Refaktorering: Forbedring av designet på eksisterende kode" av Martin Fowler](https://martinfowler.com/books/refactoring.html)
- [TSLint for statisk kodeanalyse](https://palantir.github.io/tslint/)
- [Forståelse av Kode Dårlig Lukt](https://refactoring.guru/refactoring/smells)
