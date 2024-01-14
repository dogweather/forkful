---
title:    "Gleam: Scrivere test"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/gleam/writing-tests.md"
---

{{< edit_this_page >}}

##Perché scrivere test in Gleam?

Scrivere test è un'attività critica per assicurare che il nostro codice funzioni correttamente e senza errori. Inoltre, i test possono aiutarci a identificare problemi potenziali e a prevenirli prima che si manifestino in produzione. In sintesi, scrivere test è importante per garantire la qualità del nostro codice e il corretto funzionamento delle nostre applicazioni.

##Come scrivere test in Gleam

Per scrivere test in Gleam, utilizzeremo il modulo `gleam/test` che ci offre una serie di funzioni utili per la creazione e l'esecuzione di test. Utilizzeremo la sintassi `assert()` per verificare che il nostro codice produca esattamente ciò che ci aspettiamo. Ecco un esempio di come possiamo utilizzare `gleam/test` per testare una semplice funzione che calcola la somma tra due numeri:

```Gleam
import gleam/test

fn add(x, y) {
    x + y
}

assert(gleam/test.contains(add(2, 3), 5))
```

In questo esempio, stiamo testando la funzione `add()` verificando che la somma tra 2 e 3 sia uguale a 5. Utilizzando la funzione `contains()`, possiamo verificare se il risultato della nostra funzione è uguale al valore desiderato.

##Approfondimenti sulla scrittura di test

Il modulo `gleam/test` ci offre molte altre funzioni utili per la scrittura di test, come ad esempio `assert_equal()` per verificare che due valori siano uguali o `assert_error()` per verificare che una determinata operazione sollevi un errore. Inoltre, possiamo utilizzare la funzione `describe()` per raggruppare i nostri test in categorie e rendere più leggibili i nostri report.

Inoltre, possiamo utilizzare il modulo `gleam/expect` per scrivere test in maniera ancora più espressiva e leggibile. Questo modulo ci permette di utilizzare la sintassi `expect().toEqual()` che rende i nostri test ancora più comprensibili.

##Vedi anche

-  Documentazione ufficiale di Gleam sulla scrittura dei test: https://gleam.run/book/testing.html
- Esempi di test in Gleam: https://github.com/gleam-lang/gleam/blob/main/tests/gleam_test_basic.gleam.