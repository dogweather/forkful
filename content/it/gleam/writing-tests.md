---
title:                "Gleam: Scrivere test"
programming_language: "Gleam"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Perché scrivere test in Gleam

Scrivere test del codice è una pratica fondamentale per garantire la qualità e la stabilità dei nostri programmi. In Gleam, i test ci permettono di verificare il corretto funzionamento delle nostre funzioni e di individuare eventuali errori prima che questi possano causare problemi ai nostri utenti. Inoltre, i test sono uno strumento indispensabile durante lo sviluppo di un progetto, aiutandoci a comprendere il codice e a mantenere un alto standard di pulizia e leggibilità.

## Come scrivere test in Gleam

Per scrivere test in Gleam, dobbiamo utilizzare il modulo `gleam/test` e il suo operatore `test`. Questo ci permetterà di definire una funzione di test che conterrà una o più asserzioni sul nostro codice. Vediamo un esempio:

```Gleam
import gleam/test
import MyModule

fn test_addition() {
  let result = MyModule.add(2, 3)
  let expected = 5

  test("It should correctly add two numbers", {
    assert.equal(result, expected)
  })
}
```

In questo esempio, stiamo testando la funzione `add` del nostro modulo `MyModule`, verificando che il risultato sia uguale a quello atteso. Se eseguiamo i nostri test, possiamo vedere che la funzione `add` supera il test e la funzione di test viene considerata "passed". In caso di errori, invece, il test fallirà e otterremo un messaggio di errore dettagliato.

Per ulteriori informazioni su come scrivere test in Gleam, ti consiglio di consultare la documentazione ufficiale.

## Approfondimento sui test

Scrivere test efficaci può richiedere un po' di pratica e di conoscenza, ma il tempo e lo sforzo dedicati sono ampiamente ripagati dalle numerose benefici offerti dai test. Senza test, infatti, diventa molto difficile garantire che il nostro codice funzioni correttamente e che le modifiche che apportiamo non causino regressioni.

Inoltre, scrivere test ci aiuta anche a comprendere meglio il nostro codice e a sviluppare un senso critico nei confronti dei nostri algoritmi e delle nostre scelte di design. Infine, i test ci permettono di identificare e correggere eventuali errori in modo tempestivo, contribuendo così a mantenere uno standard di qualità costante nel nostro codice.

## Vedi anche

- Documentazione ufficiale di Gleam sulla scrittura dei test (https://gleam.run/book/testing)
- Corso online su Gleam disponibile su Udemy (https://www.udemy.com/course/gleam-programming-language)
- Forum della comunità di Gleam (https://gleam.run/community)