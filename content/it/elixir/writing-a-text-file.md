---
title:                "Scrivere un file di testo."
html_title:           "Elixir: Scrivere un file di testo."
simple_title:         "Scrivere un file di testo."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Scrivere un file di testo è un'operazione comune per i programmatori in Elixir. Consiste semplicemente nel creare un file di testo con un contenuto specifico, che può essere letto o modificato successivamente. I programmatori spesso utilizzano file di testo per memorizzare informazioni come configurazioni o dati di input per il programma.

## Come fare:

```Elixir
File.write("nome_file.txt", "Contenuto del file")
```

Questo è un esempio di codice per creare un file di testo chiamato "nome_file.txt" con il contenuto "Contenuto del file". 

```Elixir
File.read("nome_file.txt")
```

Per leggere il contenuto di un file di testo esistente, è possibile utilizzare il codice sopra riportato.

## Approfondimento:

La scrittura di file di testo è un'operazione fondamentale per molte applicazioni. In passato, i programmatori dovevano creare un file di testo utilizzando linguaggi di basso livello come il C. Tuttavia, questo processo è ora semplificato grazie a linguaggi moderni come Elixir. Esistono anche librerie di terze parti come "File" che facilitano la scrittura e la lettura di file di testo.

## Vedi anche:

Per ulteriori informazioni sulla scrittura di file di testo in Elixir, è possibile consultare la documentazione ufficiale su "File": https://hexdocs.pm/elixir/File.html.