---
title:    "Elixir: Leggere un file di testo"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Perché

Stai iniziando a imparare Elixir e vuoi saperne di più su come leggere un file di testo? O forse hai bisogno di farlo per un progetto che stai sviluppando? In entrambi i casi, questo post è quello che fa per te!

## Come Fare

La lettura di un file di testo in Elixir è molto semplice grazie alla sua sintassi chiara e intuitiva. Ecco un esempio di codice usando la funzione `File.read` per leggere un file di testo e stamparne il contenuto sulla console:

```elixir
file_name = "test.txt"
{:ok, file_content} = File.read(file_name)
IO.puts file_content
```

Se il file viene trovato e letto correttamente, verrà stampato il suo contenuto sulla console. Altrimenti, si otterrà un messaggio di errore. È possibile specificare diversi parametri per la funzione `File.read` per personalizzare la lettura del file, come ad esempio il formato di encoding o il numero massimo di caratteri da leggere.

## Approfondimenti

Esistono diverse alternative alla funzione `File.read` per leggere un file di testo in Elixir, tra cui `File.stream`, che permette di leggere il file in modo più efficiente e adatti a file di grandi dimensioni. Inoltre, è possibile lavorare con file CSV utilizzando la libreria `Elixir-CSV` o leggere e scrivere file JSON utilizzando la libreria `Poison`.

Per saperne di più su queste opzioni e approfondire il concetto di lettura dei file di testo in Elixir, ti consigliamo di consultare la documentazione ufficiale di Elixir e di fare pratica scrivendo del codice.

## Vedi Anche

- Documentazione ufficiale di Elixir su File: https://hexdocs.pm/elixir/File.html
- Libreria Elixir-CSV: https://github.com/beatrichartz/elixir-csv
- Libreria Poison: https://github.com/devinus/poison