---
title:                "Att starta ett nytt projekt"
html_title:           "Arduino: Att starta ett nytt projekt"
simple_title:         "Att starta ett nytt projekt"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Påbörja Ett Nytt TypeScript Projekt: En Praktisk Guide

## Vad och Varför?
Att påbörja ett nytt projekt handlar om att skapa en strukturerad bas att bygga ditt program på. Det gör programmerarna för att fastställa en stabil grund, definiera projekt filstrukturen och skapa en effektiv arbetsflöde.

## Hur man Gör:
För att starta ett nytt TypeScript-projekt, kör du följande kod:

```TypeScript
npm init -y
npm i typescript ts-node --save-dev
```
För att kompilera dina TypeScript-filer till JavaScript, behöver du skapa en `tsconfig.json`-fil och lägga till följande konfiguration:

```TypeScript
{
    "compilerOptions": {
        "target": "es5",
        "module": "commonjs",
        "outDir": "./dist",
        "strict": true
    },
    "exclude": [
        "node_modules"
    ],
    "include": [
        "src/**/*.ts"
    ]
}
```
Du kan nu lägga till ditt startskript i package.json:

```TypeScript
"scripts": {
    "start": "ts-node ./src/index.ts"
}
```
## Djupdykning
Projektinitiering i TypeScript har utvecklats över tid och anpassats till moderna utvecklingskrav. Alternativ inkluderar att använda JavaScript direkt, eller andra kompilerade till JavaScript språk som Dart och Babel.

`tsconfig.json`-filen ger utomordentlig detaljnivå för att anpassa kompileringsprocessen till projektets behov. Vissa av dessa inställningar inkluderar "strict" för att tvinga statisk typkontroll, och "outDir" för att specificera var den kompilerade JavaScript-kod som kommer att genereras ska placeras.

## Se Även
- [TypeScript Dokumentation](https://www.typescriptlang.org/docs/)
- [ts-node](https://www.npmjs.com/package/ts-node)
- [TypeScript GitHub Repo](https://github.com/microsoft/TypeScript)
- [npm init](https://docs.npmjs.com/cli/v7/commands/npm-init)