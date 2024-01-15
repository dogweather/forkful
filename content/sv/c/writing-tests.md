---
title:                "Skrivande av tester"
html_title:           "C: Skrivande av tester"
simple_title:         "Skrivande av tester"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en viktig del av programmering som kan spara dig tid och huvudvärk i längden. Genom att skriva tester kan du kontrollera att din kod fungerar som den ska och förebygga eventuella fel eller buggar.

## Hur man gör

För att skriva tester i C behöver du först och främst ett ramverk för att köra testerna. Ett populärt val är CUnit, som är en lättanvänd och öppen källkodslösning. Sedan behöver du skapa testfiler där du definierar testfunktioner för varje del av din kod som du vill testa.

Ett exempel på en testfil för en funktion som lägger ihop två heltal:

```C 
#include <stdlib.h> 
#include <CUnit/CUnit.h> 
 
int add(int a, int b) 
{ 
  return a + b; 
} 
 
void test_add() 
{ 
  CU_assert_equal(add(5, 3), 8); 
} 

int main() 
{ 
  CU_initialize_registry(); 
  CU_pSuite suite = CU_add_suite("Addition Test Suite", NULL, NULL); 
  CU_add_test(suite, "Testing add function", test_add); 
  CU_basic_set_mode(CU_BRM_VERBOSE); 
  CU_basic_run_tests(); 
  CU_cleanup_registry(); 
 
  return CU_get_error(); 
} 
```

I det här exemplet har vi definierat en testfunktion som kollar om funktionen "add" returnerar rätt resultat när vi matar in två heltal. Vi använder även CUnits "CU_assert_equal" funktion för att jämföra det faktiska resultatet med det förväntade resultatet.

För att köra testerna behöver du kompilera både din testfil och din kodbas med CUnit. Du kan sedan köra testfilen och se om alla testfall passerar. Om något test fallerar är det dags att gå tillbaka till din kod och fixa eventuella fel.

## Djupdykning

Att skriva tester i C kan verka lite mer komplicerat jämfört med andra språk som till exempel Java eller Python. Det beror främst på att C inte har inbyggda enhetstestningsramverk och du behöver skriva dina egna tester från grunden.

En annan utmaning med att skriva tester i C är att du behöver kompilera både testfilen och din kodbas varje gång du vill köra testerna. Detta kan ta lite extra tid, men det är värt det för att se till att din kod fungerar som den ska.

## Se även

- CUnit GitHub Repository: https://github.com/ConorGil/CUnit
- Enhetstestning i C - En guide: https://stackoverflow.com/questions/12916031/unit-testing-in-c-a-simple-guide
- Jämförelse av enhetstestningsverktyg för C: https://akrzemi1.wordpress.com/2017/03/20/comparison-of-c-unit-testing-frameworks/