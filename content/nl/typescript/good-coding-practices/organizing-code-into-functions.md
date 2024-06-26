---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:17.948438-07:00
description: "Hoe te: Stel je voor dat je een basisrekenmachine maakt. In plaats van\
  \ de logica voor optellen overal waar je het nodig hebt te schrijven, maak je een\u2026"
lastmod: '2024-03-13T22:44:50.556395-06:00'
model: gpt-4-0125-preview
summary: Stel je voor dat je een basisrekenmachine maakt.
title: Code organiseren in functies
weight: 18
---

## Hoe te:
Stel je voor dat je een basisrekenmachine maakt. In plaats van de logica voor optellen overal waar je het nodig hebt te schrijven, maak je een `add` functie:

```TypeScript
function add(x: number, y: number): number {
  return x + y;
}

console.log(add(5, 7)); // Voorbeelduitvoer: 12
```

Stel nu dat we een functie nodig hebben om te vermenigvuldigen:

```TypeScript
function multiply(x: number, y: number): number {
  return x * y;
}

console.log(multiply(3, 4)); // Voorbeelduitvoer: 12
```
Merk op hoe we ons concentreren op één taak per functie? Dat is de kern van het organiseren van code.

## Diepere Duik
Historisch gezien, naarmate programmeertalen evolueerden, werden functies essentieel in het structureren van code, voortbouwend op wiskundige functies. Ze zijn een steunpilaar in procedurele programmering en leven voort in paradigma's van objectgeoriënteerd en functioneel programmeren.

Alternatieven? Je zou gewoon geen functies kunnen gebruiken, maar dat is een enkeltje naar Spaghetti Town. Of je zou voor OOP (Object-Oriented Programming) kunnen gaan en functionaliteit in methoden verpakken - die in wezen functies zijn die bij objecten horen.

Wat implementatie betreft, staat TypeScript op typen. Het definiëren van invoer- en uitvoertypen voor functies is niet alleen goede manieren; het is een must voor schone TypeScript code. Plus, met TypeScript, krijg je handige functies zoals overloads, generics, en optionele parameters om je functies te superchargen.

## Zie Ook
Bekijk deze bronnen om je functiespel te verbeteren:

- [TypeScript Handbook – Functies](https://www.typescriptlang.org/docs/handbook/2/functions.html): Je Bijbel voor TypeScript-functies.
- [Schone Code JavaScript](https://github.com/ryanmcdermott/clean-code-javascript#functions): Pas principes van Schone Code toe op je JavaScript-functies.
- [You Don’t Know JS – Scope & Sluitingen](https://github.com/getify/You-Dont-Know-JS): Krijg grip op hoe functies werken met scope en sluitingen in JavaScript.
