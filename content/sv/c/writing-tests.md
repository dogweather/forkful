---
title:    "C: Att skriva tester"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c/writing-tests.md"
---

{{< edit_this_page >}}

## Varför
Att skriva test i sin C-kod kan verka som en tidskrävande uppgift, men det kan faktiskt spara tid och frustration i det långa loppet. Genom att skriva tester kan du verifiera att din kod fungerar som den ska och minska risken för buggar och felaktigheter.

## Hur man gör
Ett enkelt sätt att komma igång med att skriva tester är att använda sig av C-enhetstester. Dessa enhetstester är små och isolerade tester som fokuserar på en specifik del av din kod. Genom att skriva dessa tester kan du säkerställa att varje liten del av din kod fungerar som den ska.

Låt oss titta på ett exempel. Vi vill skriva en funktion som beräknar medelvärdet av två tal.

```C
float average(float a, float b) {
    return (a + b) / 2;
}
```

Vi kan nu skriva ett enhetstest för denna funktion:

```C
#include <stdio.h>
#include <assert.h>

float average(float a, float b);

int main() {
    float num1 = 5.5;
    float num2 = 7.3;
    float result = average(num1, num2);

    // Jämför resultatet med det förväntade värdet 6.4
    assert(result == 6.4);

    printf("Test passed!");

    return 0;
}
```

Om testet går igenom kommer du att se "Test passed!" skrivas ut i terminalen. Om det inte går igenom, kan du enkelt identifiera och åtgärda felet i din kod.

## Djupdykning
Skrivande av enhetstester är bara en del av testningsprocessen. Du kan också skriva integrationstester för att testa hur olika delar av din kod samarbetar, samt systemtester för att verifiera att hela systemet fungerar korrekt.

Det är också viktigt att tänka på vilka scenarier och värden din kod behöver hantera och se till att dessa testas ordentligt. Att skriva testbar kod kan också underlätta processen, så tänk på att implementera designmönster som gör det enkelt att testa din kod.

## Se även
- [En översikt av enhetstester i C](https://www.geeksforgeeks.org/c-unit-testing/)
- [En översikt av integrationstester i C](https://www.softwaretestinghelp.com/c-programming-unit-integration-and-system-testing/)
- [En lista över designmönster för testbar kod i C](https://en.wikipedia.org/wiki/Category:Software_design_patterns_for_testing)