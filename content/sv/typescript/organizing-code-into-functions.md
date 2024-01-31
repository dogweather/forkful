---
title:                "Organisera kod i funktioner"
date:                  2024-01-26T01:16:11.561059-07:00
model:                 gpt-4-0125-preview
simple_title:         "Organisera kod i funktioner"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att organisera kod i funktioner innebär att dela upp din kod i återanvändbara, modulära block. Vi gör detta för att hålla saker DRY (Don’t Repeat Yourself), vilket gör koden renare, lättare att läsa, och enkel att felsöka.

## Hur man gör:
Föreställ dig att du gör en grundläggande kalkylator. Istället för att skriva logiken för addition överallt där du behöver det, skapa en `add`-funktion:

```TypeScript
function add(x: number, y: number): number {
  return x + y;
}

console.log(add(5, 7)); // Exempelutskrift: 12
```

Nu, låt oss säga att vi behöver en funktion för att multiplicera:

```TypeScript
function multiply(x: number, y: number): number {
  return x * y;
}

console.log(multiply(3, 4)); // Exempelutskrift: 12
```
Märker du hur vi fokuserar på en uppgift per funktion? Det är kärnan i att organisera kod.

## Djupdykning
Historiskt sett, när programmeringsspråk utvecklades, blev funktioner viktiga för att strukturera kod, inspirerade av matematiska funktioner. De är en hörnsten i proceduriell programmering och lever kvar inom objektorienterad och funktionell programmeringsparadigm.

Alternativ? Du kan helt enkelt välja att inte använda funktioner, men det är en enkelbiljett till Spaghettistaden. Eller så kan du gå över till OOP (Objekt-Oriented Programming) och packa funktionalitet i metoder—som i grund och botten är funktioner som tillhör objekt.

När det gäller implementering insisterar TypeScript på typer. Att definiera in- och utdatatyper för funktioner är inte bara god ton; det är ett måste för ren TypeScript-kod. Plus, med TypeScript får du finesser som överlagringar, generics och valfria parametrar för att superladda dina funktioner.

## Se också
Kolla in dessa resurser för att nivåera upp ditt funktionsspel:

- [TypeScript Handbook – Funktioner](https://www.typescriptlang.org/docs/handbook/2/functions.html): Din bibel för TypeScript-funktioner.
- [Clean Code JavaScript](https://github.com/ryanmcdermott/clean-code-javascript#functions): Tillämpa Clean Code-principer på dina JavaScript-funktioner.
- [You Don’t Know JS – Scope & Closures](https://github.com/getify/You-Dont-Know-JS): Få grepp om hur funktioner arbetar med scope och closures i JavaScript.
