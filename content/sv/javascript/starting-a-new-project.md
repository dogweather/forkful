---
title:                "Att starta ett nytt projekt"
html_title:           "Arduino: Att starta ett nytt projekt"
simple_title:         "Att starta ett nytt projekt"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/starting-a-new-project.md"
---

{{< edit_this_page >}}

---
# Starta ett nytt projekt med Javascript

## Vad och varför?

Att starta ett nytt projekt handlar om att skapa en grund att bygga ditt program på, som ett tomt blad för din kod. Programmerare gör detta för att strukturera sitt arbete, bedöma dess omfattning och förenkla underhåll.

## Hur man gör:

Här är ett exempel på hur du skapar en ny JavaScript-applikation från grunden med hjälp av Node.js och NPM (Node Package Manager).

Först installerar du Node.js och NPM på din dator. Varefter kan du skapa en ny mapp och initialisera ett nytt Node-projekt med ett kommando i terminalen:

```Javascript
mkdir mitt_nya_projekt
cd mitt_nya_projekt
npm init -y
```

Det här kommer att skapa en `package.json`-fil i din mapp, som håller reda på ditt projekts metadata och beroenden.

## Djupgående:

#### Historisk kontext:
Både Node.js och NPM, som vi använder ovan, har sina rötter i det tidiga 2000-talet när utvecklare letade efter sätt att utöka JavaScripts förmåga till server-sidan. Genom att använda dessa verktyg kan vi nu hantera hela projekt på vår dator, snarare än att vara bundna till en webbläsare.

#### Alternativ:
Även om Node.js och NPM är populära verktyg för att starta nya JavaScript-projekt, finns det många alternativ där ute. TypeScript, samtidigt som det introducerar statisk typning till JavaScript, erbjuder också projektuppsättning. Både Yarn och PNPM är alternativ till NPM för pakethantering.

#### Implementeringsdetaljer:
I vårt ovanstående exempel skapas en `package.json`-fil för att hålla koll på alla detaljer i projektet, från dess version till dess beroenden. Om du vill lägga till en ny modul till ditt projekts beroenden gör du helt enkelt det med ett `npm install`-kommando:

```Javascript
npm install express --save
```

## Se även:

- [Node.js documentation](https://nodejs.org/en/docs/)
- [NPM documentation](https://docs.npmjs.com/)
- [Yarn documentation](https://yarnpkg.com/getting-started)
- [PNPM documentation](https://pnpm.io/)
- [TypeScript documentation](https://www.typescriptlang.org/docs/)

---