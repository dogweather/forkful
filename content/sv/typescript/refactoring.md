---
title:                "Refaktorisering"
date:                  2024-01-26T03:36:26.384554-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktorisering"

category:             "TypeScript"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/refactoring.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Refaktorisering är processen att omstrukturera befintlig dator kod utan att ändra dess yttre beteende. Programmerare gör det för att göra koden renare, mer underhållsvänlig och för att minska komplexiteten, vilket gör den lättare att förstå för någon som dyker in ny.

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
