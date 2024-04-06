---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:53.221031-07:00
description: "Hoe: TypeScript, een superset van JavaScript, is ontwikkeld door Microsoft\
  \ en voor het eerst uitgebracht in oktober 2012. Het voegt statische typen toe\u2026"
lastmod: '2024-04-05T21:53:50.573915-06:00'
model: gpt-4-0125-preview
summary: TypeScript, een superset van JavaScript, is ontwikkeld door Microsoft en
  voor het eerst uitgebracht in oktober 2012.
title: Een nieuw project starten
weight: 1
---

## Hoe:
```TypeScript
// Stap 1: Installeer TypeScript globaal (indien nog niet geïnstalleerd)
npm install -g typescript

// Stap 2: Maak een nieuwe map voor je project
mkdir mijn-nieuw-project
cd mijn-nieuw-project

// Stap 3: Initialiseer een nieuw node project
npm init -y

// Stap 4: Installeer TypeScript in je project
npm install typescript --save-dev

// Stap 5: Initialiseer een TypeScript project om tsconfig.json te maken
tsc --init

// Voorbeeld tsconfig.json uitvoer (met sommige velden weggelaten voor beknopteid)
{
  "compilerOptions": {
    "target": "es5",
    "module": "commonjs",
    "strict": true,
    ...
  }
}

// Stap 6: Maak een simpel TypeScript bestand 'hello.ts'
echo 'console.log("Hallo, TypeScript!");' > hello.ts

// Stap 7: Compileer het TypeScript bestand en voer het uit
tsc hello.ts
node hello.js

// Voorbeelduitvoer
Hallo, TypeScript!
```

## Diepgaand
TypeScript, een superset van JavaScript, is ontwikkeld door Microsoft en voor het eerst uitgebracht in oktober 2012. Het voegt statische typen toe aan JavaScript, wat kan helpen fouten te vangen voor runtime en ondersteuning biedt voor IDE-functies zoals code navigatie en refactoring.

Hoewel de bovenstaande procedure npm (Node Package Manager) gebruikt, zijn er andere manieren om TypeScript projecten te beheren, zoals Yarn of pnpm. Alternatieven voor het initiëren van een TypeScript project omvatten het creëren van een project met een starterkit of het klonen van een boilerplate van repositories zoals GitHub.

De `tsconfig.json` is cruciaal; het leidt hoe de TypeScript Compiler (tsc) je TypeScript code omzet in JavaScript. Het tweaken van compiler opties laat je verschillende ECMAScript-versies, modulesystemen en meer richten, aangepast aan je projectbehoeften.

## Zie Ook
- TypeScript Officiële Documentatie: [https://www.typescriptlang.org/docs/](https://www.typescriptlang.org/docs/)
- TypeScript GitHub Repo: [https://github.com/microsoft/TypeScript](https://github.com/microsoft/TypeScript)
- TypeScript Diepgaande Verkenning: [https://basarat.gitbook.io/typescript/](https://basarat.gitbook.io/typescript/)
- Geweldig TypeScript: [https://github.com/dzharii/awesome-typescript](https://github.com/dzharii/awesome-typescript)
