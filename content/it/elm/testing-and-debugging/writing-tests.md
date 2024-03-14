---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:40.037112-07:00
description: "Scrivere test in Elm comporta la creazione di casi di test per verificare\
  \ la correttezza del tuo codice Elm, assicurandosi che si comporti come previsto.\u2026"
lastmod: '2024-03-13T22:44:43.353291-06:00'
model: gpt-4-0125-preview
summary: "Scrivere test in Elm comporta la creazione di casi di test per verificare\
  \ la correttezza del tuo codice Elm, assicurandosi che si comporti come previsto.\u2026"
title: Scrivere test
---

{{< edit_this_page >}}

## Cosa & Perché?

Scrivere test in Elm comporta la creazione di casi di test per verificare la correttezza del tuo codice Elm, assicurandosi che si comporti come previsto. I programmatori lo fanno per individuare i bug in anticipo, facilitare la manutenzione e migliorare la qualità e l'affidabilità delle loro applicazioni.

## Come fare:

Elm utilizza il pacchetto `elm-explorations/test` per scrivere test unitari e fuzz. Inizia aggiungendo il pacchetto al tuo progetto:

```elm
elm install elm-explorations/test
```

Crea un file di test, per esempio `tests/ExampleTest.elm`, e importa i moduli di testing. Ecco un semplice test che verifica una funzione `add : Int -> Int -> Int`:

```elm
module ExampleTest exposing (..)

import Expect
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "Una semplice funzione di addizione"
        [ test "Aggiungendo 2 e 3 si ottiene 5" <| 
            \_ -> add 2 3 |> Expect.equal 5
        ]

```

Per eseguire i tuoi test, avrai bisogno di `elm-test`:

```shell
npm install -g elm-test
elm-test
```

Ciò compilerà i tuoi test e stamperà i risultati nel tuo terminale. Per l'esempio sopra, l'output dovrebbe essere qualcosa di simile a:

```
TEST RUN PASSATO

Durata: 42 ms
Passati:   1
Falliti:   0
```

Per un esempio più complesso, diciamo che vuoi fare un fuzz test della funzione `add` per assicurarti che gestisca correttamente un'ampia gamma di input interi. Dovresti modificare il tuo `ExampleTest.elm` come segue:

```elm
module ExampleTest exposing (..)

import Expect
import Fuzz exposing (int)
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "Testando add con il fuzzing"
        [ fuzz int "Fuzz test su add con interi casuali" <| 
            \int1 int2 -> add int1 int2 |> Expect.equal (int1 + int2)
        ]
```

Esegui di nuovo `elm-test` per vedere i fuzz test in azione. L'output varierà con input casuali ma i test riusciti indicheranno l'assenza di fallimenti:

```
TEST RUN PASSATO

Durata: 183 ms
Passati:   100
Falliti:   0
``` 

Questi esempi mostrano come scrivere ed eseguire semplici test unitari e fuzz in Elm, utilizzando il pacchetto `elm-explorations/test`. Testare è una parte vitale del processo di sviluppo, aiutando a garantire che le tue applicazioni Elm siano affidabili e mantengano un'alta qualità.
