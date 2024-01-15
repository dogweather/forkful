---
title:                "Att skriva tester"
html_title:           "TypeScript: Att skriva tester"
simple_title:         "Att skriva tester"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Varför ska man bry sig om att skriva tester när man redan har en fungerande kod? Det är en vanlig fråga som många programmerare ställer sig. Men faktum är att tester är en väsentlig del av den moderna utvecklingsprocessen. Genom att skriva tester kan man säkerställa att koden fungerar som den ska och undvika oväntade buggar som kan uppstå när man gör ändringar i koden.

Att skriva tester är också ett av de bästa sätten att förbättra kvaliteten på sin kod. Genom att testa sin kod kan man hitta och åtgärda eventuella fel och förbättra användarupplevelsen. Dessutom kan tester fungera som dokumentation av koden och underlätta för andra utvecklare att förstå och bidra till projektet.

## Hur man skriver tester i TypeScript

Att skriva tester i TypeScript är enkelt. Man kan använda biblioteket "Jasmine" för att skriva enhetstester. Här är ett exempel på hur en simpel test skulle kunna se ut:

````TypeScript
describe('Calculation functions', () => {
  it('should add two numbers correctly', () => {
    const result = add(3, 5);
    expect(result).toBe(8);
  });
});
````

I detta exempel har vi en describe-funktion som innehåller ett antal olika tester. Inuti detta block använder vi en it-funktion för att specificera vad som förväntas hända. Vi kör funktionen vi vill testa och sedan lovar vi att resultatet ska vara det vi förväntar oss, genom att använda expect-funktionen.

## Fördjupning

När man skriver tester är det viktigt att tänka på att testa både positiva och negativa scenarier. Detta innebär att man bör testa både förväntade och oväntade användningsfall. Det är också bra att testa gränsvärden för att säkerställa att koden hanterar dem på ett korrekt sätt.

En annan viktig aspekt av tester är att de bör vara isolerade. Det betyder att varje test bör testa en specifik funktion eller ett specifikt scenario, utan att påverkas av andra delar av koden. Detta gör det enklare att felsöka och underhålla tester i framtiden.

Det finns också olika typer av tester som man kan skriva i TypeScript, såsom integrations- och regressionstester. Det är upp till utvecklaren att bestämma vilken typ av tester som är lämpliga för varje projekt.

## Se även

- [Jasmine dokumentation](https://jasmine.github.io/)
- [Hur man tester sin kod i TypeScript](https://www.sitepoint.com/unit-test-javascript-mocha-chai/)
- [En introduktion till automatiserade tester i TypeScript](https://www.codeproject.com/Articles/1249585/Automated-Tests-in-TypeScript)