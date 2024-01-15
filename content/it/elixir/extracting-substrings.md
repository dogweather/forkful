---
title:                "Estrazione di sottostringhe"
html_title:           "Elixir: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché
Se stai lavorando con stringhe di testo in Elixir, potresti trovarti nella situazione in cui devi estrarre una sottostringa, ovvero una porzione di testo più piccola dalla stringa principale. Questo può essere utile per effettuare confronti, manipolare i dati o semplicemente per ottenere una parte specifica di una stringa.

## Come Fare
Per estrarre una sottostringa in Elixir, puoi utilizzare la funzione `String.slice`. Questa funzione accetta due argomenti: la stringa di origine e un intervallo di indici per indicare la porzione di testo che desideri ottenere. Ad esempio, se vogliamo estrarre i primi tre caratteri di una stringa, possiamo scrivere il seguente codice:

```Elixir
stringa = "Ciao a tutti!"
String.slice(stringa, 3, 0) # Output: "Cia"
```

In questo esempio, `stringa` è la nostra stringa di origine, `3` è il primo indice (compreso) e `0` è il secondo indice (escluso). Inoltre, possiamo anche specificare un solo indice se desideriamo estrarre una sottostringa a partire da quel punto. Ad esempio:

```Elixir
stringa = "Ciao a tutti!"
String.slice(stringa, 5) # Output: "a tutti!"
```

In questo caso, la sottostringa viene estratta a partire dall'indice 5 fino alla fine della stringa.

## Approfondimento
Per ottenere una migliore comprensione di `String.slice`, è utile sapere che questa funzione utilizza l'indice `0` come punto di partenza. Ciò significa che se abbiamo una stringa di 10 caratteri, l'ultimo indice utilizzabile sarà 9. Inoltre, possiamo anche utilizzare numeri negativi per indicare gli indici a partire dalla fine della stringa. Ad esempio:

```Elixir
stringa = "Elixir è fantastico!"
String.slice(stringa, -9, -1) # Output: "fantastico"
```

In questo esempio, stiamo estraendo la sottostringa che va dall'indice -9 (escluso) all'indice -1 (incluso), ovvero gli ultimi 9 caratteri della stringa.

## Vedi Anche
- [Documentazione di Elixir su String.slice](https://hexdocs.pm/elixir/String.html#slice/3)
- [Tutorial su come manipolare le stringhe in Elixir](https://dev.to/brpaz/working-with-strings-in-elixir-3dbc)
- [Esercizi pratici per imparare Elixir](https://exercism.io/tracks/elixir/exercises)