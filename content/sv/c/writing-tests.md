---
title:    "C: Skrivande av tester"
keywords: ["C"]
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en viktig del av att utveckla pålitliga, robusta och hållbara C-program. Genom att skriva tester kan du upptäcka och åtgärda buggar innan de når produktionen och säkerställa att koden fungerar som den ska. Det sparar inte bara tid och resurser i det långa loppet, utan det ger också en säkerhetsnivå som ger dig förtroende för ditt eget arbete.

## Hur man gör

Att skriva tester kan verka lite skrämmande först, men det är faktiskt ganska enkelt att komma igång. Här är ett exempel på hur du kan skriva ett enkelt testfall för en grundläggande funktion:

```C
#include <stdio.h>

int add(int a, int b) {
    return a + b;
}

int main() {
    // Skapa två variabler och lagrar det förväntade resultatet
    int num1 = 10;
    int num2 = 5;
    int expected_result = 15;

    // Anropa add funktionen med våra två variabler
    int result = add(num1, num2);

    // Jämför det faktiska resultatet med det förväntade
    if (result == expected_result) {
        printf("Add funktionen fungerar korrekt!\n");
    } else {
        printf("Bugg hittades i add funktionen!\n");
    }

    return 0;
}
```

När du kör detta program bör du få utskriften "Add funktionen fungerar korrekt!". Om du ändrar värdet på num1, num2 eller expected_result till felaktiga värden, kommer programmet att upptäcka det och meddela dig att det finns en bugg i din kod. Detta är en enkel och användbar metod för att verifiera att dina funktioner fungerar som de ska.

Ett annat sätt att testa din kod är att använda enhetstester. Dessa är isolerade tester som fokuserar på en specifik del av din kod och dess output. Genom att skapa flera enhetstester för varje funktion i ditt program kan du få en bättre täckning av din kod och upptäcka eventuella buggar tidigt.

## Djupdykning

När du skriver tester är det viktigt att du faktiskt testar de möjliga scenarierna som din kod kan ställas inför. Du bör också vara medveten om eventuella gränsvärden eller hörnfall som kan orsaka problem. Ofta är det de mindre och förbisedda delarna av koden som leder till de största problemen.

Det är också viktigt att testa både positiva och negativa scenarier. Positiva tester utför koden som det är tänkt att göras och förväntar sig det förväntade resultatet. Negativa tester testar hur koden hanterar felaktiga eller ogiltiga input och ser till att den inte kraschar eller ger oväntade resultat.

En annan viktig aspekt av att skriva tester är att göra dem skalbara och underhållbara. Om du gör förändringar i din kod, måste du se till att dina tester också uppdateras för att återspegla dessa ändringar. Detta säkerställer att om dina tester en gång har godkänts, kommer de fortsätta att fungera korrekt även efter eventuella ändringar.

## Se också

Här är några länkar för mer information om testning i C-programmering:

- [En introduktion till enhetstestning i C](https://medium.com/dev-genius/an-introduction-to-unit-testing-in-c-503d899d81c9)
- [Test-Driven Development med C körs på Nginx](https://www.nginx.com/blog/test-driven-development-nginx-unit-piotr-sharov/)
- [CUnit - Ett enhetstestramverk för C](http://cunit.sourceforge.net/)
- [Unity - Ett annat populärt enhetstestramkurk för C](http://www.throwtheswitch.org/unity)

Lycka till med testningen av d