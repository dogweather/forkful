---
date: 2024-01-20 18:04:42.184339-07:00
description: "Hur man g\xF6r: F\xF6r att kickstarta ett TypeScript-projekt, anv\xE4\
  nd f\xF6ljande kommandon."
lastmod: '2024-03-13T22:44:37.656815-06:00'
model: gpt-4-1106-preview
summary: "F\xF6r att kickstarta ett TypeScript-projekt, anv\xE4nd f\xF6ljande kommandon."
title: "Att p\xE5b\xF6rja ett nytt projekt"
weight: 1
---

## Hur man gör:
För att kickstarta ett TypeScript-projekt, använd följande kommandon:

```TypeScript
// Installerar TypeScript globalt
npm install -g typescript

// Skapar en ny mapp för projektet
mkdir my-new-project && cd my-new-project

// Initierar ett nytt npm-projekt
npm init -y

// Lägger till en tsconfig.json fil för TypeScript configuration
tsc --init

// Skapar en grundläggande app.ts fil
echo "console.log('Hello, TypeScript!');" > app.ts

// Kompilera TypeScript till JavaScript
tsc

// kör den kompilerade JavaScript-filen
node app.js
```

Exempel på utdata:
```
Hello, TypeScript!
```

## Fördjupning
TypeScript släpptes först av Microsoft 2012. Det erbjuder statisk typning till JavaScript, vilket kan förbättra kodkvaliteten och minska antalet körningstidsfel. Alternativ till att starta ett projekt från grunden inkluderar att använda starters eller scaffolding-verktyg som 'create-react-app' för React-projekt med TypeScript-stöd. I praktiken innebär att starta ett nytt TypeScript-projekt att du konfigurerar `tsconfig.json` noggrant för att matcha dina behov, vilket kan inkludera jämförelsen av kompileringsalternativ, inkludering av polyfills och bibliotekstyper för kompatibilitet och optimering av byggprocessen.

## Se också
- Den officiella TypeScript-dokumentationen: [https://www.typescriptlang.org/docs/](https://www.typescriptlang.org/docs/)
- `tsconfig.json` dokumentation: [https://www.typescriptlang.org/tsconfig](https://www.typescriptlang.org/tsconfig)
- TypeScript Gitter, en chatt för TypeScript-utvecklare: [https://gitter.im/Microsoft/TypeScript](https://gitter.im/Microsoft/TypeScript)
- GitHub-repo för TypeScript: [https://github.com/microsoft/TypeScript](https://github.com/microsoft/TypeScript)
- Ett populärt verktyg för att skapa nya projekt är Yeoman: [http://yeoman.io/](http://yeoman.io/)
