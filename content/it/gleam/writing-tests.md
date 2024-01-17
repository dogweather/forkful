---
title:                "Scrivere test"
html_title:           "Gleam: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Scrivere i test è una pratica comune per i programmatori che consiste nel creare piccoli pezzi di codice che verificano la correttezza delle funzioni principali. Questo aiuta a garantire che il programma funzioni correttamente e che non ci siano bug.

## Come fare:

Per creare un test in Gleam, utilizziamo la funzione `test` seguita dal nome del nostro test e una funzione che controlla se il nostro codice produce il risultato atteso. Ecco un esempio:

```
Gleam
  test "addition test" {
    assert.equal(add(3, 4), 7)
  }
```

Questo test verifica che la funzione `add` restituisca 7 quando viene passato 3 e 4 come argomenti. Se il risultato non è quello atteso, il test fallirà e ci segnalerà il risultato ottenuto.

## Approfondimento

Scrivere test è importante perché ci aiuta a identificare eventuali problemi nel nostro codice prima che venga utilizzato in produzione. Ciò risulta particolarmente utile quando si apportano modifiche al codice, in quanto i test possono aiutarci a individuare dove si è verificato un errore.

Una possibile alternativa ai test è l'utilizzo di strumenti di debugging, ma non sempre questi sono in grado di individuare tutti i problemi nel nostro codice. Invece, i test ci aiutano a individuare eventuali errori in modo più preciso e completo.

Per quanto riguarda l'implementazione dei test in Gleam, è tornato in auge grazie all'approccio funzionale della programmazione, che rende più semplice scrivere codice testabile e creare funzioni pure senza effetti collaterali.

## Vedi anche:

- [Documentazione ufficiale di Gleam sugli unit test](https://gleam.run/documentation/guides/unit_tests)
- [Articolo su Medium che spiega l'importanza dei test nel codice](https://medium.com/javascript-scene/why-i-use-tape-instead-of-mocha-so-should-you-6aa105d8eaf4)