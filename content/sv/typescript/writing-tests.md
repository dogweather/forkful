---
title:                "TypeScript: Skriva tester"
programming_language: "TypeScript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Varför
Att skriva tester är en viktig del av programmering eftersom det hjälper till att säkerställa att koden fungerar som den ska och undviker onödiga buggar och fel. Det är också ett bra sätt att dokumentera koden och underlätta för andra att förstå och bidra till projektet.

## Hur man gör det
Att skriva tester i TypeScript är enkelt och effektivt. Här är ett exempel på hur man kan skapa en grundläggande testmiljö med Jasmine:

```TypeScript
// Importera nödvändiga moduler
import { Calculator } from "./calculator";
import { expect } from "chai";
import "jasmine";

// Skapa ett testfall
describe("Calculator", () => {
    let calculator: Calculator;

    beforeEach(() => {
        // Skapa en instans av calculator innan varje test
        calculator = new Calculator();
    });

    it("should add two numbers correctly", () => {
        expect(calculator.add(2, 2)).to.equal(4);
    });
});
```

När testen körs med hjälp av Jasmine, kommer output att visa om testen har passerats eller misslyckats. Detta är ett enkelt exempel, men man kan skriva flera testfall för att täcka olika funktioner och scenarier.

## Djupdykning
För att skriva effektiva tester i TypeScript finns det några viktiga saker att tänka på. Först och främst är det viktigt att namnge testen på ett beskrivande sätt så att andra lätt kan förstå vad de testar. Dessutom är det viktigt att täcka olika fall, både för positiva och negativa scenarier, för att säkerställa att koden fungerar som den ska. Man kan också använda olika testramverk och verktyg för att underlätta skrivandet och körningen av testen.

## Se även
Här är några användbara länkar för att lära sig mer om att skriva test i TypeScript:

- [Jasmine dokumentation](https://jasmine.github.io/)
- [Mocha testramverk](https://mochajs.org/)
- [Chai assertions bibliotek](https://www.chaijs.com/)
- [Karma test runner](https://karma-runner.github.io/latest/index.html)