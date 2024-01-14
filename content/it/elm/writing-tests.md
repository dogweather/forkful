---
title:    "Elm: Scrittura di test"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Perché scrivere test in Elm?

Scrivere test è uno strumento molto utile per garantire la qualità del codice che scriviamo. Ci permette di identificare eventuali errori o bug prima che il nostro codice sia in produzione, risparmiando tempo e riducendo i rischi. Inoltre, scrivere test ci aiuta a mantenere il codice organizzato e comprensibile, facilitando la manutenzione nel lungo termine.

## Come scrivere test in Elm

Per scrivere test in Elm possiamo utilizzare il modulo `elm-test`, che ci permette di scrivere test in modo semplice e intuitivo. Vediamo un esempio pratico:

```Elm
-- Definiamo una funzione che restituisce true se il numero passato come argomento è pari
isEven : Int -> Bool
isEven number =
  number % 2 == 0

-- Utilizziamo la funzione `describe` per definire il nome del nostro test e `test` per specificare il comportamento atteso
tests : Test
tests =
  describe "isEven" [
    test "should return true if number is even" <|
      \() ->
        Expect.equal (isEven 4) True
  ]
```

Eseguendo il test con il comando `elm-test` dovremmo ottenere il seguente output:

```
PASS 1 test passed

Finished in 0.004 seconds
```

## Approfondimento sui test in Elm

Scrivere test in Elm ci permette di essere più sicuri del nostro codice, ma è importante ricordare alcune best practice:

- Scrivere test precisi e specifici, in modo da poter identificare facilmente eventuali errori
- Utilizzare la funzione `Expect.equal` per confrontare il valore attuale con quello atteso
- Utilizzare il comando `elm-test` regolarmente per eseguire i test e garantire che il codice continui a funzionare correttamente

## Vedi anche

- [Documentazione su `elm-test`](https://package.elm-lang.org/packages/elm-explorations/test/latest/) 
- [Esempi di test in Elm](https://github.com/elm-explorations/test#examples) 
- [Best practices per scrivere test in Elm](https://thoughtbot.com/blog/how-we-write-tests-in-elm)