---
title:    "Elm: Generazione di numeri casuali"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Perché

Quando si programma in Elm, a volte è necessario generare numeri casuali per simulare un'interazione casuale o per aggiungere un elemento di casualità a un gioco o un'animazione. In questo articolo, impareremo come generare numeri casuali in Elm e ci immergeremo nel funzionamento interno di questo processo.

## Come Fare

Per generare numeri casuali in Elm, è necessario utilizzare il modulo `Random` e le sue funzioni `generate` e `step`. Ad esempio, se si desidera generare un numero intero casuale compreso tra 1 e 10, è possibile utilizzare il seguente codice:

```Elm
Random.generate NewNumber (Random.step (Random.int 1 10))
```

Questa funzione restituirà un oggetto `Cmd Msg`, che può essere gestito all'interno dell'architettura di un programma Elm.

Per visualizzare il numero casuale generato, è possibile utilizzare un `Sub Msg` come questo:

```Elm
subscriptions model =
    Sub.map NewNumber (Random.generate NewNumber (Random.step (Random.int 1 10)))
```

Questo sottoscrittore chiamerà la funzione `NewNumber` ogni volta che viene generato un nuovo numero casuale, e il valore verrà aggiunto al modello del programma. Da qui, è possibile utilizzare il numero casuale in qualsiasi modo si desideri, ad esempio nella visualizzazione o in una funzione di aggiornamento.

## Profondità della Generazione di Numeri Casuali

La generazione di numeri casuali in Elm è possibile grazie al modulo `Random`, che utilizza algoritmi basati sui numeri casuali del sistema operativo sottostante. Ciò significa che diversi sistemi operativi o esecuzioni dello stesso programma possono produrre risultati diversi. Inoltre, è possibile specificare un seme di generazione per ottenere sempre lo stesso risultato.

È importante notare che i numeri casuali generati tramite il modulo `Random` non sono veramente casuali, ma sono solo simili a cose che accadono nel mondo reale. Ciò significa che non dovrebbero essere utilizzati per scopi critici, come la sicurezza.

## Vedi Anche

- Documentazione sul modulo `Random` di Elm: https://package.elm-lang.org/packages/elm/random/latest/
- Esempio di utilizzo del modulo `Random` in un gioco di Blackjack in Elm: https://github.com/w0rm/elm-blackjack/blob/master/src/Random.hs
- Tutorial su come generare numeri casuali in Elm: https://medium.com/@julianknodt/elm-iso-rand-javascript-c693ef19c6d1