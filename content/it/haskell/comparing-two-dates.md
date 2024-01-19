---
title:                "Confronto tra due date"
html_title:           "Elixir: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

Il confronto tra due date è un'operazione comune che permette di stabilire se una data è anteriore, successiva o uguale ad un’altra. Questo è essenziale in vari contesti di programmazione come nel pianificare eventi o nel calcolo di intervalli di tempo.

## Come fare:

In Haskell, puoi utilizzare le funzioni incorporate per manipolare e confrontare le date. Osserva questo esempio di base:

```Haskell
import Data.Time.Calendar

data1 = fromGregorian 2021 12 16
data2 = fromGregorian 2022 01 01

compareContents = compare data1 data2

main = print compareContents
```
L'output sarà: 
```Haskell
LT
```
Questo perchè 'LT' indica che `data1` è meno di `data2`.

## Approfondimenti

Haskell usa il tipo di dato `Day` per rappresentare le date. Questo tipo è una 'Newtype' basata sull´intero, dove il valore rappresenta il numero di giorni trascorsi dal 'day zero', ovvero il primo Gennaio 1 del 1858.

Ci sono diverse alternative per confrontare le date in Haskell, come l'uso di librerie esterne come `time` o `date`. Tuttavia, le funzioni integrate forniscono un modo semplice ed efficiente per farlo.

La funzione `compare` è una funzione predefinita in Haskell che ti consente di confrontare i valori di due elementi. Questo funziona solo se i tipi degli elementi che stai cercando di comparare implementano l'interfaccia `Ord`, che, per fortuna, è implementata dal tipo `Day`.

## Approfondisci Portando a Conoscenza

Ecco alcuni link per approfondire l'argomento:

1. [Documentazione Haskell ufficiale](https://www.haskell.org/documentation/)
2. [Pacchetto Time su Hackage](https://hackage.haskell.org/package/time)
3. [Corso di introduzione a Haskell](https://www.futurelearn.com/courses/functional-programming-haskell)