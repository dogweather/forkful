---
date: 2024-01-26 03:36:26.384554-07:00
description: "Refaktorisering \xE4r processen att omstrukturera befintlig dator kod\
  \ utan att \xE4ndra dess yttre beteende. Programmerare g\xF6r det f\xF6r att g\xF6\
  ra koden renare,\u2026"
lastmod: '2024-03-13T22:44:37.664315-06:00'
model: gpt-4-0125-preview
summary: "Refaktorisering \xE4r processen att omstrukturera befintlig dator kod utan\
  \ att \xE4ndra dess yttre beteende."
title: Refaktorisering
weight: 19
---

## Hur man gör:
Tänk dig en TypeScript funktion som har sett bättre dagar - den är lite rörig och kan behöva lite ömhet och omsorg:

```typescript
function userInfo(data: any): string {
    return "User Info: " + data.name + ", " + data.age + ", " + data.email + ";" ;
}
```
Refaktoriserad kan detta se ut så här:

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

Det andra exemplet är mer robust, genom att utnyttja TypeScripts typsystem med ett `interface` för att undvika potentiella körningstidsfel och förbättra läsbarheten.

## Djupdykning
Refaktorisering är inte ett modernt koncept; det har utvecklats med programmeringen, och blev mer formaliserat med utgivningen av Martin Fowlers bok "Refactoring: Improving the Design of Existing Code" år 1999. Det är avgörande i en Agile utvecklingsmiljö, vilket underlättar anpassningsbara kodändringar. Alternativ till manuell refaktorisering inkluderar automatiserade verktyg som TSLint eller TypeScript's eget språkserver som kan föreslå eller till och med utföra vissa refaktoreringsuppgifter åt dig. Genomförandet detaljer involverar vanligtvis att känna igen "kodlukt", såsom dubblettkod, långa metoder eller stora klasser, och tillämpa mönster för att avhjälpa—som att extrahera metoder, flytta till mer lämpliga klasser eller använda enklare konstruktioner. Dessa mönster är nyckeln till att förstå hur och varför man refaktoriserar.

## Se även
- [Boken "Refactoring: Improving the Design of Existing Code" av Martin Fowler](https://martinfowler.com/books/refactoring.html)
- [TSLint för statisk kodanalys](https://palantir.github.io/tslint/)
- [Att förstå Kodlukt](https://refactoring.guru/refactoring/smells)
