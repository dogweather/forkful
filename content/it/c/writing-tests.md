---
title:                "Scrivere test"
date:                  2024-01-19
simple_title:         "Scrivere test"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? 
Quando scriviamo test, stiamo verificando che parti specifiche del nostro codice funzionino come previsto. I programmatori fanno questo per evitare bug, semplificare la manutenzione e garantire che aggiunte future non compromettano funzionalità esistenti.

## How to:
Ecco un semplice test con `assert` in C:

```C
#include <assert.h>

int somma(int a, int b) {
    return a + b;
}

int main() {
    assert(somma(2, 2) == 4);
    assert(somma(0, 0) == 0);
    assert(somma(-1, -1) == -2);
    return 0; // Se tutti gli assert passano, il programma finisce qui.
}
```
Output in caso di successo (nessun output, programma termina senza errori). Se un `assert` fallisce, il programma termina mostrando l'assert errato.

## Deep Dive
I test automatici hanno origini nei primi giorni dell'informatica, ma si sono evoluti con i framework di test come CUnit, MinUnit, e altri. Questi offrono più funzionalità rispetto ad `assert` base, come generazione di report e organizzazione dei test in suite. Implementarli richiede includere questi framework nel tuo progetto e seguire la loro struttura per scrivere e eseguire i test.

## See Also
- Unit Testing Frameworks in C: https://libcheck.github.io/check/, http://cunit.sourceforge.net/
- Best Practices in Software Testing: https://www.guru99.com/software-testing-best-practices.html
- Intro to Test Driven Development (TDD) in C: https://www.ibm.com/developerworks/library/l-tdd/
