---
title:                "Att påbörja ett nytt projekt"
html_title:           "TypeScript: Att påbörja ett nytt projekt"
simple_title:         "Att påbörja ett nytt projekt"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Varför

Att starta ett nytt projekt kan vara ett spännande och givande äventyr för en utvecklare. Det ger dig möjlighet att utmana dig själv och skapa något unikt som kan hjälpa många människor. Dessutom är TypeScript ett språk som växer i popularitet och kan ge dig möjlighet att lära dig nya tekniker och utveckla din kunskap.

## Så här gör du

För att börja ett nytt TypeScript-projekt, behöver du först installera Node.js och TypeScript-compiler. Sedan kan du följa dessa enkla steg:

1. Skapa en ny mapp för ditt projekt och navigera in i den.
2. Öppna en terminal och skriv ```npm init``` för att skapa en `package.json`-fil.
3. Installera TypeScript genom att skriva ```npm install typescript --save-dev```.
4. Skapa en fil som heter `tsconfig.json` med följande innehåll:

```TypeScript
{
  "compilerOptions": {
    "target": "es6",
    "module": "commonjs",
    "outDir": "dist",
    "strict": true,
    "esModuleInterop": true
  },
  "include": [
    "./src/**/*"
  ],
  "exclude": [
    "./node_modules"
  ]
}
```

5. Skapa en mapp som heter `src` och lägg till en fil med namnet `index.ts` som innehåller din första TypeScript-kod.
6. Navigera till roten av din mapp och skriv ```npx tsc``` för att kompilera din kod till JavaScript och skapa en mapp med namnet `dist`.
7. Nu kan du börja koda! För att köra din kod, skriv ```node dist/index.js```.

## Djupdykning

Att starta ett nytt projekt är en viktig process och det finns många saker att tänka på. Här är några tips som kan hjälpa dig:

- Planera ditt projekt noggrant innan du börjar koda för att undvika onödig omkodning.
- Utvärdera olika designmönster och välj den som passar ditt projekt bäst.
- Använd verktyg som Git för att hantera din kod och samarbeta med andra utvecklare.
- Glöm inte bort att testa din kod noggrant för att försäkra dig om att den fungerar som den ska.

## Se även

- [https://www.typescriptlang.org/](https://www.typescriptlang.org/) - officiell hemsida för TypeScript.
- [https://github.com/Microsoft/TypeScript](https://github.com/Microsoft/TypeScript) - officiell GitHub-sida för TypeScript.
- [https://www.nodejs.org/](https://www.nodejs.org/) - officiell hemsida för Node.js.
- [https://www.npmjs.com/](https://www.npmjs.com/) - officiell hemsida för npm (Node Package Manager).