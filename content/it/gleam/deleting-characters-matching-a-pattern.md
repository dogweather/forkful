---
title:                "Eliminazione di caratteri corrispondenti ad un modello"
html_title:           "Gleam: Eliminazione di caratteri corrispondenti ad un modello"
simple_title:         "Eliminazione di caratteri corrispondenti ad un modello"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Stai cercando un modo semplice ed efficiente per eliminare i caratteri da una stringa che corrispondono ad uno specifico pattern? La soluzione potrebbe essere più facile di quanto pensi, grazie alla funzione `delete_matching` del linguaggio di programmazione Gleam.

## Come Fare

Gleam è un linguaggio di programmazione funzionale staticamente tipizzato che punta a semplificare la creazione di applicazioni affidabili e performanti. Per utilizzare la funzione `delete_matching` dovrai seguire questi semplici passaggi:

1. Definisci una variabile che rappresenti la stringa su cui vuoi operare:

```
let stringa = "Questo-e-un-test per rimuovere i caratteri.";
```

2. Definisci il pattern dei caratteri che desideri eliminare, utilizzando espressioni regolari:

```
let pattern = ~r"\W"; // Rimuove tutti i caratteri non alfanumerici
```

3. Utilizza la funzione `delete_matching` per creare una nuova stringa senza i caratteri corrispondenti al pattern:

```
let nuova_stringa = delete_matching(stringa, pattern); // Il risultato sarà "Questoeuntestperrimuovereicaratteri"
```

4. Puoi stampare il risultato per verificare il corretto funzionamento:

```
io.println(nuova_stringa); // Output: "Questoeuntestperrimuovereicaratteri"
```

## Approfondimento

La funzione `delete_matching` accetta due argomenti: la stringa da modificare e il pattern dei caratteri da eliminare. Il pattern può essere definito utilizzando una stringa di espressione regolare o un valore di tipo `Regex`.

Inoltre, è possibile utilizzare il parametro opzionale `count` per specificare il numero massimo di corrispondenze da eliminare. Ad esempio, se volessi eliminare solo le prime 2 occorrenze del nostro pattern, potresti scrivere:

```
let nuova_stringa = delete_matching(stringa, pattern, ~s[ count: 2 ]);
```

## Vedi Anche

- Documentazione ufficiale di Gleam - [https://gleam.run/](https://gleam.run/)
- Espressioni regolari in Gleam - [https://gleam.run/book/tour/regular-expressions.html](https://gleam.run/book/tour/regular-expressions.html)
- Tutorial sull'utilizzo di Gleam per i principianti - [https://gleam.run/book/tour/](https://gleam.run/book/tour/)