---
title:                "TypeScript: Utskrift av felsökningsutdata"
simple_title:         "Utskrift av felsökningsutdata"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Det är en vanlig praxis för utvecklare att skriva ut debug-utdata i sina kod för att hjälpa till att felsöka och optimera programmet. Detta är särskilt användbart när man arbetar med komplexa system eller löser problem som kräver en djupare förståelse av koden. I denna bloggpost kommer vi att titta på varför utskrift av debug-utdata är en ovärderlig teknik i TypeScript-programmering.

## Hur man gör det

För att skriva ut debug-utdata i TypeScript kan du använda funktionen `console.log()` eller `console.debug()`. Här är ett exempel på hur du kan använda `console.log()`:

```TypeScript
let num1: number = 10;
let num2: number = 5;
console.log("Num1: " + num1);
console.log("Num2: " + num2);
console.log("Summan av num1 och num2 är: " + (num1 + num2));
```

Output:

```
Num1: 10 
Num2: 5
Summan av num1 och num2 är: 15
```

Du kan också använda placeholders för att göra din kod mer dynamisk. Här är ett exempel på hur du kan använda placeholders:

```TypeScript
let name: string = "Maria";
let age: number = 25;
console.debug("Hej, mitt namn är %s och jag är %d år gammal.", name, age);
```

Output:

```
Hej, mitt namn är Maria och jag är 25 år gammal.
```

## Djupdykning

Att skriva ut debug-utdata hjälper dig att spåra vad som händer i ditt program och kontrollera värdet på olika variabler och uttryck. Detta är särskilt användbart när du letar efter buggar eller optimerar din kod. Genom att skriva ut specifika värden kan du undersöka vilka delar av koden som körs och jämföra dem med det förväntade beteendet.

En annan fördel med att skriva ut debug-utdata är att det kan hjälpa dig att förstå hur din kod fungerar och vilka metoder som är mest effektiva. Genom att se vad som händer på olika punkter i din kod kan du identifiera flaskhalsar och förbättra prestandan.

En viktig punkt att komma ihåg när du skriver ut debug-utdata är att se till att ta bort eller kommentera ut alla utskrifter före produktion. Annars kan det påverka prestandan och orsaka onödig overhead i din kod.

## Se också

Här är några länkar för att lära dig mer om att skriva ut debug-utdata i TypeScript:

- [https://www.typescriptlang.org/docs/handbook/intro-to-js-ts.html#printing-to-the-console](https://www.typescriptlang.org/docs/handbook/intro-to-js-ts.html#printing-to-the-console)
- [https://stackify.com/typescript-debugging-tips/](https://stackify.com/typescript-debugging-tips/)
- [https://codeburst.io/typescript-debugging-with-type-assertions-aa1c3751e959](https://codeburst.io/typescript-debugging-with-type-assertions-aa1c3751e959)