---
title:                "Skriva tester"
date:                  2024-01-19
html_title:           "Arduino: Skriva tester"
simple_title:         "Skriva tester"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (Vad & Varför?)
Att skriva tester innebär att koda små program som kontrollerar att annan kod fungerar som avsett. Programmerare gör det för att upptäcka buggar tidigt, säkerställa kodkvalitet och förenkla framtida underhåll.

## How to: (Hur man gör:)
```C
#include <assert.h>

// En enkel funktion som adderar två tal
int add(int a, int b) {
    return a + b;
}

// Testfunktion för att verifiera 'add'-funktionen
void test_add() {
    assert(add(2, 3) == 5);
    assert(add(-1, 1) == 0);
    // Lägg till fler tester vid behov...
}

int main() {
    test_add(); // Kör testfunktionen
    printf("Alla tester gick igenom!\n");
    return 0;
}
```
Output:
```
Alla tester gick igenom!
```

## Deep Dive (Djupdykning)
Historiskt sett växte testningen fram som nödvändig del i mjukvaruutveckling för att hantera ökande komplexitet. Alternativ till `assert` inkluderar större testramverk som Unity och CMocka. Dessa ramverk erbjuder mer sofistikerade funktioner såsom mockning och setup/teardown för tester.

## See Also (Se också)
- [Unity Test Framework](https://www.throwtheswitch.org/unity)
- [CMocka](https://cmocka.org/)
