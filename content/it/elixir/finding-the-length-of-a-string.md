---
title:                "Trovare la lunghezza di una stringa"
html_title:           "Arduino: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Trova la lunghezza di una stringa in Elixir

## Cos'è e perché?

Calcolare la lunghezza di una stringa significa determinare il numero di caratteri nella stringa. I programmatori fanno questo perché è un'operazione comune nel manipolare e analizzare il testo nei programmi.

## Come fare:

In Elixir, la funzione `String.length/1` può essere utilizzata per trovare la lunghezza di una stringa. Ecco un esempio:

```Elixir
IO.puts String.length("Ciao, mondo!")
```

Questo restituirà `12`, il numero di caratteri nella stringa fornita, incluso lo spazio e il punto esclamativo.

```Elixir
12
```

## Approfondimento

Storicamente, il calcolo della lunghezza di una stringa è un'operazione fondamentale in molti linguaggi di programmazione. In Elixir, `String.length/1` funziona con stringhe UTF-8 e conta i caratteri Unicode, non i byte.

Un'alternativa è usare `byte_size/1`, che restituisce il numero di byte che la stringa occupa. Questo può essere diverso dal numero di caratteri se la stringa contiene caratteri Unicode che occupano più di un byte.

```Elixir
IO.puts byte_size("Ciao, mondo!")  # restituirà 13, non 12
```

Ad esempio, il carattere Unicode `é` occupa due byte, quindi `byte_size("é")` restituirebbe `2`, mentre `String.length("é")` restituirebbe `1`.

Le performance di `String.length/1` sono O(n), il che significa che il tempo necessario per eseguire la funzione aumenta linearmente con la lunghezza della stringa.

## Vedi anche

- La documentazione ufficiale di Elixir fornisce maggiori dettagli sulla [funzione String.length/1](https://hexdocs.pm/elixir/String.html#length/1).

- Per un'esplorazione più approfondita delle stringhe di Elixir, dai un'occhiata alla [guida ufficiale di Elixir sulle stringhe](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html). 

- Ecco un [articolo informativo](https://www.jungledisk.com/blog/2019/07/03/understanding-utf-8-and-unicode/) che spiega la differenza tra UTF-8 e Unicode.