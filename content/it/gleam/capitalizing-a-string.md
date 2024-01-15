---
title:                "Capitalizzazione di una stringa"
html_title:           "Gleam: Capitalizzazione di una stringa"
simple_title:         "Capitalizzazione di una stringa"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Capitare una stringa (cioè farla iniziare con una lettera maiuscola) può essere utile in molte situazioni, come per esempio quando si vuole migliorare la leggibilità di un testo o per applicare delle regole di capitalizzazione specifiche.

## Come fare

Per capitare una stringa in Gleam, possiamo utilizzare la funzione `String.capitalize` che prende come argomento la stringa da capitare e restituisce una nuova stringa con la prima lettera maiuscola.

Esempio di codice:

```Gleam
let nome = "mario"
let nome_capitalizzato = String.capitalize(nome)
```

Output:

```Gleam
"Mario"
```

In questo modo, abbiamo creato una nuova stringa `nome_capitalizzato` con la prima lettera maiuscola.

## Approfondimento

La funzione `String.capitalize` non solo fa diventare maiuscola la prima lettera di una stringa, ma gestisce anche le stringhe unicode in modo corretto. Inoltre, è anche possibile utilizzare la funzione `String.split` per dividere una stringa in base ad un carattere specifico e successivamente capitalizzare ogni parte della stringa divisa.

## Vedi anche

- [La documentazione ufficiale di Gleam](https://gleam.run/documentation)
- [Un tutorial su come utilizzare le stringhe in Gleam](https://www.freecodecamp.org/news/gleam-programming-language-everything-you-need-to-know/)
- [Altri articoli su programmazione con Gleam in italiano](https://medium.com/translate-me/traduzione-italiana-per-un-articolo-sul-comparison-between-elixir-lang-vs-gleam-lang-373ef9056015)