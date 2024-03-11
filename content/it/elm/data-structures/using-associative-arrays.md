---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:03.043227-07:00
description: "Gli array associativi, o come li chiama Elm, Dizionari, mappano le chiavi\
  \ ai valori in modo tale da rendere il recupero, l'inserimento e l'eliminazione\u2026"
lastmod: '2024-03-11T00:14:16.913877-06:00'
model: gpt-4-0125-preview
summary: "Gli array associativi, o come li chiama Elm, Dizionari, mappano le chiavi\
  \ ai valori in modo tale da rendere il recupero, l'inserimento e l'eliminazione\u2026"
title: Utilizzo di array associativi
---

{{< edit_this_page >}}

## Cosa e Perché?

Gli array associativi, o come li chiama Elm, Dizionari, mappano le chiavi ai valori in modo tale da rendere il recupero, l'inserimento e l'eliminazione dei valori estremamente rapidi. Sono la tua prima scelta quando hai bisogno di tenere traccia di cose senza un ordine preciso, come le preferenze degli utenti o gli elenchi di inventario.

## Come fare:

In Elm, lavori con i Dizionari nel modulo `Dict`, quindi esploriamo un rapido esempio:

```Elm
import Dict exposing (Dict)

-- Inizializzazione di un dizionario con chiavi String e valori Int
exampleDict : Dict String Int
exampleDict = Dict.fromList [("apple", 5), ("banana", 2), ("orange", 8)]

-- Aggiungere o aggiornare un valore
updatedDict = Dict.insert "grape" 10 exampleDict

-- Recuperare un valore (notare il tipo Maybe, poiché la chiave potrebbe non essere presente)
fruitCount : Maybe Int
fruitCount = Dict.get "apple" updatedDict

-- Rimuovere una coppia chiave-valore
finalDict = Dict.remove "banana" updatedDict

-- Convertire un dizionario di nuovo in una lista
dictToList = Dict.toList finalDict
```

Output di esempio quando si visualizza `dictToList`:

```Elm
[("apple", 5), ("grape", 10), ("orange", 8)]
```

Questo dimostra le operazioni di base: creazione, aggiornamento, accesso e iterazione su un Dizionario.

## Approfondimento

I Dizionari in Elm internamente utilizzano una struttura nota come albero AVL - un tipo di albero binario di ricerca auto-bilanciante. Questa scelta raggiunge un equilibrio tra garantire che operazioni come l'inserimento, il recupero e la rimozione abbiano buone prestazioni (complessità temporale logaritmica) e mantenere la semplicità nella gestione dei dati.

Nonostante i punti di forza del `Dict` di Elm, non è una soluzione adatta a tutti gli usi. Per collezioni ordinate o che devono essere iterate sequenzialmente, List o Array potrebbero essere più appropriati. Inoltre, quando si lavora con un insieme fisso di chiavi note, l'uso di tipi personalizzati (la versione di Elm degli enum) potrebbe offrire maggiore sicurezza dei tipi e un'intenzione più chiara nel tuo codice.

Nell'ecosistema di Elm, `Dict` offre un modo affidabile per gestire collezioni di coppie chiave-valore in cui le chiavi sono uniche e l'ordine non ha importanza. Mentre potrebbero emergere strutture più nuove o più sofisticate, il modulo `Dict` rimane uno strumento fondamentale nel kit dello sviluppatore Elm per la sua semplicità e efficienza nel gestire gli array associativi.
