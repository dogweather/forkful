---
title:                "Lettura di un file di testo."
html_title:           "Elixir: Lettura di un file di testo."
simple_title:         "Lettura di un file di testo."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Cos&#39;è e perché?

Lettura di un file di testo è il processo di leggere e interpretare il contenuto di un file di testo nella memoria del computer. Questa operazione è essenziale per programmi che richiedono l'accesso a dati memorizzati in un file di testo, come ad esempio la lettura di un file di configurazione o l'analisi di un file di log. È una delle operazioni più comuni che i programmatori eseguono per interagire con il sistema di file.

## Come fare:

```elixir
# Leggere un file di testo
File.read("nome_file.txt")

# Ottenere il contenuto di un file di testo come una lista di righe
File.read!("nome_file.txt") |> String.split("\n")

# Leggere il contenuto di un file di testo come una stringa codificata
File.read!("nome_file.txt") |> String.downcase()
```

Output:

```elixir
"contenuto_del_file"
["riga 1", "riga 2", "riga 3"]
"il contenuto del file è stato trasformato in minuscolo"
```

## Approfondimento

La lettura di un file di testo è stata un'operazione fondamentale sin dai primi giorni della programmazione. Originariamente, il file system non era così diffuso come oggi e i programmi spesso dovevano leggere o scrivere su nastri magnetici o altre dispositivi di archiviazione. Negli anni successivi, i sistemi operativi hanno fornito librerie e API per semplificare la lettura dei file di testo e dei file in generale.

Elixir fornisce più di una funzione per leggere un file di testo, in modo da poter scegliere la più adatta alle tue esigenze. Inoltre, se hai bisogno di leggere file di formati diversi da quello di testo, puoi utilizzare librerie di terze parti come NimbleCSV o Poison per leggere file CSV o JSON.

## Vedi anche

- [File](https://hexdocs.pm/elixir/File.html) - Documentazione ufficiale della funzione di lettura di Elixir.
- [NimbleCSV](https://hexdocs.pm/nimble_csv/NimbleCSV.html) - Libreria di terze parti per leggere e scrivere file CSV in Elixir.
- [Poison](https://hexdocs.pm/poison/Poison.html) - Libreria di terze parti per leggere e scrivere file JSON in Elixir.