---
title:                "Ricerca e sostituzione di testo"
html_title:           "Elixir: Ricerca e sostituzione di testo"
simple_title:         "Ricerca e sostituzione di testo"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

La ricerca e la sostituzione di testo sono operazioni comuni nella programmazione, soprattutto quando si lavora con stringhe di testo. Grazie all'utilizzo di Elixir, è possibile eseguire queste operazioni in modo semplice ed efficiente, risparmiando tempo e riducendo gli errori.

## Come fare

Per eseguire la ricerca e la sostituzione di testo in Elixir, è possibile utilizzare la funzione `String.replace/3`. Questa funzione richiede tre argomenti: la stringa di input, il pattern da cercare e il pattern di sostituzione. Ad esempio:

```Elixir
iex> String.replace("Ciao, mondo!", "Ciao", "Hello")
"Hello, mondo!"
```

In questo esempio, la funzione sostituisce il testo "Ciao" con "Hello" nella stringa di input "Ciao, mondo!". La funzione restituisce la stringa modificata come risultato.

La funzione `String.replace/3` è anche molto utile quando si lavora con espressioni regolari. Ad esempio, se si vuole sostituire tutte le vocali di una stringa con il simbolo "*", è possibile utilizzare il seguente codice:

```Elixir
iex> String.replace("Elixir", ~r/[aeiou]/, "*")
"*l*x*r"
```

In questa esempio, viene utilizzata un'espressione regolare per cercare tutte le vocali nella stringa "Elixir" e sostituirle con "*". È importante notare che il pattern di sostituzione può anche essere una funzione, permettendo una maggiore flessibilità nell'esecuzione della sostituzione.

## Approfondimenti

La funzione `String.replace/3` è solo una delle numerose funzioni fornite da Elixir per eseguire la ricerca e la sostituzione di testo. Altre funzioni utili sono `String.replace_leading/3` e `String.replace_trailing/3`, che permettono di sostituire rispettivamente solo il primo o l'ultimo caso trovato del pattern cercato.

Anche i pattern di sostituzione possono essere più complessi di un semplice stringa, grazie all'utilizzo di funzioni o espressioni regolari. Inoltre, Elixir offre anche funzioni per eseguire il confronto delle stringhe senza tenere conto delle maiuscole e minuscole (`String.upcase/1` e `String.downcase/1`).

Inoltre, è importante ricordare che la funzione `String.replace/3` non modifica la stringa di input originale, ma restituisce sempre una nuova stringa. Se si vuole modificare la stringa originale, è necessario assegnare il risultato della funzione alla variabile desiderata.

## Vedi anche

- [Documentazione ufficiale di Elixir](https://hexdocs.pm/elixir/String.html#replace/3)
- [Tutorial di ricerca e sostituzione di testo in Elixir](https://www.freecodecamp.org/news/elixir-tutorial-search-replace-text/)