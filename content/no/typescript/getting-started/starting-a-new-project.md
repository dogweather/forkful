---
date: 2024-01-20 18:04:50.785692-07:00
description: "How to: For \xE5 sette i gang et nytt TypeScript-prosjekt, trenger du\
  \ Node.js og NPM (Node Package Manager) installert. Deretter bruker du f\xF8lgende\u2026"
lastmod: '2024-03-13T22:44:40.532928-06:00'
model: gpt-4-1106-preview
summary: "For \xE5 sette i gang et nytt TypeScript-prosjekt, trenger du Node.js og\
  \ NPM (Node Package Manager) installert."
title: "\xC5 starte et nytt prosjekt"
weight: 1
---

## How to:
For å sette i gang et nytt TypeScript-prosjekt, trenger du Node.js og NPM (Node Package Manager) installert. Deretter bruker du følgende kommandoer:

1. Opprett en ny mappe for prosjektet ditt:
```bash
mkdir mitt-nye-typescript-prosjekt
cd mitt-nye-typescript-prosjekt
```

2. Initialiser et nytt NPM-prosjekt:
```bash
npm init -y
```
Dette skaper en `package.json` fil som holder prosjektets metadata.

3. Installer TypeScript som en utvikleravhengighet:
```bash
npm install typescript --save-dev
```

4. Opprett en `tsconfig.json` fil for å konfigurere TypeScript-opsjoner:
```bash
npx tsc --init
```

5. Lage en `src` mappe og en enkel TypeScript-fil `index.ts`:
```bash
mkdir src
echo "console.log('Hei, Verden!');" > src/index.ts
```

6. Kompilere TypeScript-koden og kjøre den med Node.js:
```bash
npx tsc
node dist/index.js
```

Sample output:
```
Hei, Verden!
```

## Deep Dive
TypeScript utviklet seg fra JavaScript for å tilby statisk type-sjekking, som kan hindre mange vanlige feil ved kjøretid. Historisk sett utviklet Microsoft TypeScript i 2012, og siden da har det blitt et kraftig verktøy som mange i JavaScript-fellesskapet har adoptert. 

Alternativer til TypeScript inkluderer rene JavaScript-prosjekter, Flow (fra Facebook), eller Dart (fra Google). Valget avhenger av prosjektets krav og utviklerens preferanser. Når det kommer til implementasjon av TypeScript, spiller `tsconfig.json` en viktig rolle ved å tillate finjustering av kompilatoren, som inkluderer modulsystem, målversjon av ECMAScript, og mange andre opsjoner som styrer hvordan koden kompileres.

## See Also
- TypeScript Offisielle Dokumentasjon: [https://www.typescriptlang.org/docs/](https://www.typescriptlang.org/docs/)
- NPM Dokumentasjon: [https://docs.npmjs.com/](https://docs.npmjs.com/)
- Node.js Offisielle Nettsted: [https://nodejs.org/](https://nodejs.org/)
