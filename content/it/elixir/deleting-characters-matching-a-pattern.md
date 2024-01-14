---
title:                "Elixir: Eliminare i caratteri corrispondenti a un modello"
simple_title:         "Eliminare i caratteri corrispondenti a un modello"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

##Perché
Eliminare i caratteri che corrispondono a un determinato modello può essere molto utile per pulire e organizzare il proprio codice, soprattutto quando si lavora su progetti di grandi dimensioni. Inoltre, può essere una tecnica efficace per risolvere problemi specifici all'interno di un programma.

##Come fare
Per eliminare i caratteri che corrispondono a un modello in Elixir, possiamo utilizzare la funzione `String.replace/4` che prende quattro argomenti: la stringa da cui eliminare i caratteri, il modello da cercare, la stringa di sostituzione e il numero massimo di elementi da sostituire. Di seguito un esempio di come utilizzare questa funzione:

```Elixir
iex> stringa = "Ciao mondo, benvenuto!"
"Ciao mondo, benvenuto!"
iex> String.replace(stringa, "o", "")           
"Cia mond, benvenut!"
```
Come possiamo notare, tutti i caratteri "o" sono stati eliminati dalla stringa originale.

Possiamo anche utilizzare espressioni regolari per creare modelli più complessi da utilizzare nella funzione `String.replace/4`. Ad esempio, se vogliamo eliminare tutte le vocali dalla stringa, possiamo utilizzare `[^aeiou]` come modello, che indica "tutto tranne le vocali". Ecco un esempio di come utilizzare questa espressione regolare:

```Elixir
iex> stringa = "Ciao mondo, benvenuto!"
"Ciao mondo, benvenuto!"
iex> String.replace(stringa, ~r/[^aeiou]/, "")   
"iao oo, eeuo!"
```

## Approfondimento
La funzione `String.replace/4` utilizza l'algoritmo di ricerca e sostituzione di Boyer-Moore per trovare e sostituire i caratteri corrispondenti al modello. Questo algoritmo è molto veloce e efficiente, rendendo l'operazione di eliminazione dei caratteri molto rapida anche su stringhe di grandi dimensioni.

Inoltre, è importante notare che la funzione `String.replace/4` restituisce una nuova stringa anziché modificare la stringa originale. Questo è importante da considerare quando si lavora con stringhe immutabili in Elixir.

## Vedi anche
- [Elixir String.replace documentation](https://hexdocs.pm/elixir/String.html#replace/4)
- [Boyer-Moore pattern matching algorithm](https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_string-search_algorithm)