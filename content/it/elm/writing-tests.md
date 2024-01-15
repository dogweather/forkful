---
title:                "Scrivere test"
html_title:           "Elm: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Perché
Ci sono molte buone ragioni per scrivere test quando si programma in Elm. In primis, ci aiutano a verificare che il nostro codice faccia quello che ci aspettiamo. Inoltre, ci permettono di identificare errori o bug in modo più rapido e preciso.

## Come
Scrivere test in Elm è semplice e intuitivo. Basta utilizzare il modulo `Test` all'inizio del nostro file e definire una funzione `test` che accetta un nome e una funzione di test come parametri. Ecco un esempio:

```Elm
import Test

test "Somma corretta" =
    expect <| 2 + 3 == 5
```

In questo caso, stiamo verificando se la somma tra 2 e 3 è effettivamente uguale a 5. Utilizzando l'operatore `==` possiamo specificare quale risultato ci aspettiamo dalla funzione di test. Possiamo anche utilizzare l'operatore `!=` per verificare che il risultato sia diverso da quello atteso.

Possiamo eseguire i nostri test utilizzando la funzione `Test.run` all'interno della funzione `main` del nostro programma. Questo ci fornirà un output a riga di comando indicando il numero di test che sono stati eseguiti e il loro esito.

## Deep Dive
Scrivere test in Elm non si limita solo all'utilizzo di `expect`. Possiamo anche utilizzare le funzioni `fuzz` e `fuzz2` per testare il nostro codice utilizzando valori casuali. Ad esempio:

```Elm
test "Test casuale" =
    fuzz3 "Formato della data" (\\day month year -> day / month == 0) 
```

In questo caso, stiamo testando una funzione che dovrebbe verificare se il giorno è divisibile per il mese. Utilizzando `fuzz3`, abbiamo specificato che vogliamo che il nostro test utilizzi valori casuali per `day`, `month` e `year`. Possiamo anche utilizzare `fuzz2` e `fuzz4` per testare funzioni con diversi parametri.

Inoltre, possiamo utilizzare il modulo `Expect` per avere una maggiore precisione nei nostri test. Ad esempio, possiamo utilizzare `expectEqual` per verificare se due valori sono effettivamente uguali o `expectNotEqual` per verificare che siano diversi.

## See Also
Se vuoi saperne di più su come scrivere test in Elm, consulta questi utili link:

- [Documentazione Elm sul testing](https://guide.elm-lang.org/testing/)
- [Esempi di test in Elm](https://github.com/elm-community/elm-test/tree/master/examples)
- [Video tutorial su Elm e il testing](https://www.youtube.com/watch?v=c8xMfQYPHso)