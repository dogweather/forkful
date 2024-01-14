---
title:                "Elixir: Estrazione di sottostringhe."
simple_title:         "Estrazione di sottostringhe."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

La funzione di estrarre sottostringhe è fondamentale per manipolare testi e stringhe in Elixir. Grazie alla sua semplicità e versatilità, questa funzione può aiutare a risparmiare tempo e rendere il tuo codice più efficiente.

## Come Fare

Per estrarre una sottostringa da una stringa esistente in Elixir, possiamo utilizzare la funzione `String.slice/2`. Questa funzione prende come argomenti una stringa e un intervallo di caratteri da estrarre. Ad esempio, se vogliamo estrarre i primi tre caratteri da una stringa, il nostro codice sarebbe il seguente:

```Elixir
stringa = "Ciao mondo"
String.slice(stringa, 0..2)
```

Questo ci restituirebbe la sottostringa "Cia".

Possiamo anche utilizzare un altro argomento opzionale per specificare lo step della selezione dei caratteri. Ad esempio, se vogliamo estrarre una sottostringa con i caratteri pari da una stringa, possiamo utilizzare il seguente codice:

```Elixir
stringa = "Elixir è fantastico"
String.slice(stringa, 0..-1, 2)
```

Questo ci restituirebbe la sottostringa "Elr sn".


## Approfondimento

La funzione `String.slice/2` è basata sulla funzione `String.substr/3`, che a sua volta utilizza la libreria standard di Elixir `StringBuilder`. Ciò significa che possiamo sfruttare le potenzialità di `StringBuilder` per ottenere prestazioni ottimali.

Inoltre, è importante notare che la funzione `String.slice/2` restituisce una nuova stringa e non modifica la stringa originale. Questo rende il nostro codice più sicuro e facile da leggere.

## Vedi Anche

- [Documentazione di Elixir su String.slice/2](https://hexdocs.pm/elixir/String.html#slice/2)
- [Tutorial su Elixir e manipolazione di stringhe](https://elixirschool.com/it/lessons/basics/binaries-strings-and-char-lists/#manipolazione-di-stringhe)
- [Libreria StringBuilder di Elixir](https://hexdocs.pm/elixir/String.Builder.html)