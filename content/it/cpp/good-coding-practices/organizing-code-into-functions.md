---
title:                "Organizzazione del codice in funzioni"
aliases:
- /it/cpp/organizing-code-into-functions.md
date:                  2024-01-26T01:09:04.980036-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizzazione del codice in funzioni"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Cosa e perché?
Suddividere il codice in funzioni significa suddividere il codice in parti più piccole e riutilizzabili. Lo facciamo per evitare ripetizioni, rendere il nostro codice leggibile e semplificare il debug e il testing. Avere funzioni ben organizzate può essere come avere una scatola di strumenti ordinatamente etichettati, pronti per l'uso e la condivisione.

## Come Fare:
Prendiamo un compito comune: calcolare l'area di un cerchio. Invece di scrivere la stessa formula ogni volta, la incapsuliamo in una funzione.

```C++
#include <iostream>
#define PI 3.14159

double calculateCircleArea(double radius) {
    return PI * radius * radius;
}

int main() {
    double r = 5.0;
    std::cout << "Area del cerchio con raggio " << r << " è " << calculateCircleArea(r) << std::endl;
    return 0;
}
```

Esempio di output:
```
Area del cerchio con raggio 5 è 78.5397
```

## Approfondimento
Storicamente, procedure e funzioni sono state l'asse portante della programmazione strutturata, promossa negli anni '60 per combattere i problemi del "codice spaghetti" nei precedenti linguaggi di programmazione imperativa. Alternative come la OOP (Programmazione Orientata agli Oggetti) vanno oltre associando queste funzioni a strutture dati. In C++, hai funzioni regolari, metodi di classe (inclusi metodi statici), lambda e funzioni template, ciascuna con diversi benefici. Implementare funzioni ben organizzate di solito implica l'aderenza a principi come DRY ("Don't Repeat Yourself", Non Ripetere Te Stesso) e SRP (Principio di Singola Responsabilità), il che significa che ogni funzione fa una sola cosa, ma la fa bene.

## Vedi Anche
Per saperne di più sulle funzioni in C++:
- https://en.cppreference.com/w/cpp/language/functions
- https://www.learncpp.com/cpp-tutorial/77-introduction-to-functions/

Per i principi di progettazione relativi alle funzioni:
- https://en.wikipedia.org/wiki/Single-responsibility_principle
- https://en.wikipedia.org/wiki/Don%27t_repeat_yourself

Scopri di più su lambda e uso avanzato delle funzioni:
- https://www.cprogramming.com/c++11/c++11-lambda-closures.html
- https://isocpp.org/wiki/faq/cpp14-language#lambda-captures
