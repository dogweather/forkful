---
title:                "Scrivere test"
html_title:           "C: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/writing-tests.md"
---

{{< edit_this_page >}}

## Che cos'è e perché lo facciamo?
Scrivere dei test è il processo di creare delle piccole porzioni di codice che verificano il funzionamento di parti specifiche del nostro programma. È importante farlo perché ci permette di individuare eventuali errori nel nostro codice in modo rapido ed efficiente.

## Come si fa:
Nel seguente esempio, creiamo una semplice funzione che verifica se un numero è pari o dispari:

```C
#include <stdio.h>

// Funzione che verifica se un numero è pari o dispari
// Restituisce 1 se è pari, 0 se è dispari
int even_or_odd(int num) {
  if (num % 2 == 0) {
    return 1;
  } else {
    return 0;
  }
}

int main() {
  int num = 5;
  if (even_or_odd(num)) {
    printf("%d è un numero pari\n", num);
  } else {
    printf("%d è un numero dispari\n", num);
  }
  return 0;
}
```

L'output di questo esempio sarà: ```5 è un numero dispari```

## Deep Dive:
Scrivere dei test è una pratica molto diffusa nel mondo della programmazione. Infatti, già negli anni '60 si iniziò a parlare di questo argomento quando si cominciarono a utilizzare i cosiddetti "test di unità" per verificare il corretto funzionamento del codice.

Oggi ci sono diverse alternative per scrivere test, come ad esempio i "test di integrazione", che verificano il corretto funzionamento di intere parti del programma.

Per implementare dei test efficaci è importante seguire alcune buone pratiche, come ad esempio scrivere test semplici ed indipendenti l'uno dall'altro e utilizzare strumenti specifici per questo scopo.

## Vedi anche:
- [JUnit](https://junit.org/junit5/) - uno dei più famosi framework per scrivere test in Java e altre lingue.
- [PyTest](https://docs.pytest.org/en/latest/) - framework per scrivere test in Python.