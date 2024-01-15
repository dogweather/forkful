---
title:                "Lettura di un file di testo"
html_title:           "Elixir: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti dover leggere un file di testo in Elixir. Potresti voler estrarre dati da un file di dati, leggere un file di configurazione o semplicemente leggere un file di testo per visualizzare il suo contenuto.

## Come fare

Per leggere un file di testo in Elixir, devi prima aprire il file utilizzando la funzione `File.open/2`. Questa funzione prende come argomenti il percorso del file e la modalità di apertura. 

```elixir
File.open("myfile.txt", [:utf8, :read])
|> IO.read()
```

Questa semplice sequenza di codice aprirà il file "myfile.txt" in modalità di lettura e restituirà il suo contenuto come stringa. Per poter utilizzare questa stringa, puoi salvarla in una variabile o utilizzarla direttamente nelle tue operazioni.

Se hai bisogno di leggere il file riga per riga, puoi utilizzare la funzione `IO.reads/2`. Questa funzione restituirà un elenco di righe del file. 

```elixir
File.open("myfile.txt", [:utf8, :read])
|> IO.reads(:line)
```

Puoi anche leggere un numero specifico di caratteri dal file utilizzando la funzione `IO.read/2`. Ad esempio, se vuoi leggere solo i primi 100 caratteri, puoi utilizzare il codice seguente:

```elixir
File.open("myfile.txt", [:utf8, :read])
|> IO.read(100)
```

## Deep Dive

Se vuoi approfondire il processo di lettura di un file di testo in Elixir, puoi anche esaminare le funzioni del modulo `File` utilizzate per questa attività. Ad esempio, puoi utilizzare la funzione `File.stream!/2` per leggere il contenuto del file in un flusso che ti permetterà di accedere alle righe del file una alla volta. Inoltre, puoi specificare la modalità di apertura del file utilizzando le opzioni fornite dal modulo `:options`. 

```elixir
File.stream!("myfile.txt", [:utf8, :read])
|> Stream.take(5)
|> Enum.to_list()
```

## Vedi anche

Per ulteriori informazioni su come lavorare con i file in Elixir, puoi consultare la documentazione ufficiale del linguaggio e altri articoli su come utilizzare le funzioni del modulo `File`. Ecco alcuni link utili:

- [Documentazione ufficiale di Elixir](https://hexdocs.pm/elixir/File.html)
- [Utilizzo del modulo File in Elixir](https://www.devdungeon.com/content/directory-listing-and-file-i-o-elixir)
- [Introduzione alla lettura e scrittura di file in Elixir](https://david.elorio.me/elixir-file-io-in-under-five-minutes/)