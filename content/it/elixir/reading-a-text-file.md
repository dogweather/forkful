---
title:                "Lettura di un file di testo"
html_title:           "C: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Leggere un file di testo in Elixir
A volte, si ha bisogno di esplorare il mondo di Elixir. Ecco una guida rapida sulla lettura di un file di testo con Elixir. 

## Cosa & Perché?
Leggere un file di testo significa recuperare ed elaborare i dati presenti all'interno del file. I programmatori lo fanno per manipolare tali dati, generare output, alimentare algoritmi e molto altro ancora.

## Come fare:
Vediamo come si legge un file di testo in Elixir. E' molto semplice grazie alla funzione `File.read/1`.

```Elixir
{:ok, contenuto} = File.read("mio_file.txt")
IO.puts contenuto
```
In questo esempio, `"mio_file.txt"` è il file che desideri leggere. Il contenuto del file viene stampato sulla console.

## Approfondimento:
La funzione `File.read/1` in Elixir risale al linguaggio Erlang su cui si basa Elixir ed è fondamentale per le operazioni di I/O. Come alternative, potresti considerare l'uso di `File.stream!/1` se hai a che fare con file molto grandi o se hai bisogno di un controllo più dettagliato durante la lettura.

In termini di implementazione, la funzione `File.read/1` in realtà utilizza le funzionalità I/O del sistema operativo sottostante, il che significa che la sua esecuzione può variare leggermente tra sistemi operativi diversi.

## Per Approfondire:
Per ulteriori informazioni su `File.read/1` e le altre funzioni di lettura file in Elixir, visita i link seguenti:
- [Elixir's official documentation on File module](https://hexdocs.pm/elixir/File.html)
- [Handling files and IO in Elixir](https://dev.to/wagslane/handling-files-and-io-in-elixir-jk5)