---
title:    "TypeScript: Att skriva tester"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Varför skriva tester i TypeScript?

Att skriva tester är en viktig del av en utvecklares arbetsuppgifter. Det bidrar till att säkerställa att koden fungerar som tänkt och att den fortsätter att fungera även efter eventuella ändringar. I TypeScript, ett populärt programmeringsspråk för webbutveckling, kan du skriva tester med hjälp av verktyget Jasmine. Det ger dig en strukturerad och organiserad metod för att testa din kod.

## Hur man skriver tester i TypeScript

För att börja skriva tester i TypeScript, måste du först installera Jasmine via npm. Sedan kan du skapa en ny fil för dina tester och importera Jasmine biblioteket. Nedan följer ett exempel på hur du kan skriva ett enkelt test som kollar om en funktion returnerar rätt värde:

```TypeScript
import { add } from "./calculator";

describe("add function", () => {
    it("should return the sum of two numbers", () => {
        expect(add(2, 3)).toEqual(5);
    });
});
```

I exemplet ovan importerar vi en funktion som heter "add" från en fil som heter "calculator". Därefter skapar vi en "describe" funktion som beskriver vad testet ska göra. Inuti denna funktion skapar vi en "it" funktion som specificerar vad som förväntas hända. Slutligen använder vi "expect" funktionen för att verifiera om resultatet från "add" funktionen stämmer överens med det förväntade värdet.

## Deep Dive

Nu när du har en grundläggande förståelse för hur man skriver tester i TypeScript, kan du utforska mer avancerade koncept som t.ex. mockning och stubbing. Mockning tillåter dig att simulera en extern resurs eller funktion för att testa din kod oberoende av den. Stubbing å andra sidan, låter dig definiera ett förutbestämt värde eller resultat för en funktion. Detta är speciellt användbart när du behöver testa avgränsade delar av din kod separat.

## Se även

- [Jasmine](https://jasmine.github.io)
- [TypeScript Official Website](https://www.typescriptlang.org)
- [Mocka och Stubbning med TypeScript](https://medium.com/@aravindsharma66/mocking-and-stubbing-in-typescript-a7596417f8f2)