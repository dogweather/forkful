---
title:                "Scrivere test"
html_title:           "Haskell: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Cosa è e perché?

Scrivere test di programma significa creare codice che verifica e prova le funzionalità del nostro software. I programmatori lo fanno per garantire che il loro codice funzioni correttamente e per trovare eventuali errori prima che i loro programmi vengano utilizzati da utenti reali.

## Come si fa:

```Haskell
-- Importa il modulo di test di Haskell
import Test.HUnit

-- Definisci una funzione da testare
somma :: Int -> Int
somma x = x + 5

-- Definisci una lista di test
tests = TestList [
  "Somma 3" ~: somma 3 ~?= 8, -- Verifica che la somma di 3 sia 8
  "Somma 7" ~: somma 7 ~?= 12 -- Verifica che la somma di 7 sia 12
]

-- Esegui i test e stampa l'output
main = do
  counts <- runTestTT tests -- Il risultato dei test viene assegnato a 'counts'
  print counts -- Stampa il numero di test superati e falliti
```

```
Cases: 2  Tried: 2  Errors: 0  Failures: 0
Counts {cases = 2, tried = 2, errors = 0, failures = 0}
```

## Approfondimento:

Scrivere test è una pratica comune tra i programmatori per garantire la qualità del loro codice e facilitare la manutenzione. In passato, i programmatori dovevano testare manualmente ogni parte del loro software, ma con l'avvento dei moder