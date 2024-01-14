---
title:                "C: Att skriva tester"
programming_language: "C"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en viktig del av en utvecklares arbete. Det hjälper till att bekräfta att koden fungerar som den ska och att den inte bryts av eventuella förändringar i kodbasen. Genom att skriva tester kan du också enklare hitta och lösa eventuella buggar, vilket i slutändan sparar tid och pengar.

## Så här gör du

För att skriva tester i C måste du först förstå hur testning fungerar i denna programmeringsspråk. En vanlig metod är att använda ett testramverk, som till exempel CUnit. Genom detta ramverk kan du skriva testfall som kontrollerar att kodens olika funktioner fungerar korrekt.

Här är ett exempel på hur man skriver ett enkelt testfall i C unit:

```C
#include <stdio.h>
#include <CUnit/CUnit.h>
#include <CUnit/Basic.h>

int add(int x, int y) {
    return x + y;
}

void test_add() {
    CU_ASSERT_EQUAL(add(2, 2), 4);
    CU_ASSERT_EQUAL(add(5, -2), 3);
}

int main() {
    CU_initialize_registry();
    
    CU_pSuite suite = CU_add_suite("Add Test Suite", NULL, NULL);
    
    CU_add_test(suite, "add test", test_add);
    
    CU_basic_set_mode(CU_BRM_VERBOSE);
    CU_basic_run_tests();
    CU_cleanup_registry();
    
    return 0;
}
```

När detta test körs, kommer det att kontrollera att funktionen "add" returnerar rätt resultat för olika indata. Om testet lyckas, kommer du att få utskriften "CUnit: no failures" från programmet.

## Djupdykning

Att skriva tester kan vara en tidskrävande process, men det finns några viktiga aspekter att tänka på för att göra uppgiften enklare. För det första bör du se till att testa så många möjliga vägar genom koden för att säkerställa att alla funktioner fungerar korrekt. Det är också viktigt att skriva testfall som täcker eventuella buggar som du vet finns i koden.

Ett annat tips är att vara noggrann med namngivningen av dina testfall. Detta gör det enklare att förstå vad som testas och vilken typ av fel som uppstår om testet misslyckas.

Slutligen är det viktigt att använda testdriven utveckling (TDD) för att säkerställa att nya kodändringar inte bryter befintlig fungerande kod. Genom att skriva testfall innan du skriver koden kan du upptäcka eventuella problem innan de blir svårare och dyrare att åtgärda.

## Se även

- [CUnit testramverk](https://frankmorgner.github.io/cunit/)
- [TDD koncept och tekniker](https://www.softwaretestinghelp.com/test-driven-development-tdd/)
- [En guide till enhetstestning i C](https://www.section.io/engineering-education/writing-unit-tests-in-c/)