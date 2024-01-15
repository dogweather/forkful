---
title:                "Scrivere un file di testo"
html_title:           "Elixir: Scrivere un file di testo"
simple_title:         "Scrivere un file di testo"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Scrivere un file di testo può sembrare un'operazione banale, ma è un processo essenziale per gestire e organizzare i dati all'interno di un programma. Inoltre, può anche essere utile per creare documentazione o report leggibili da altri.

## Come Fare

In Elixir, è possibile scrivere un file di testo utilizzando il modulo `File`. Vediamo un esempio pratico:

```elixir
# Creiamo un nuovo file di testo chiamato "mionuovofile.txt"
File.write("mionuovofile.txt", "Questo è il mio primo file di testo!")
```

In questo esempio, stiamo utilizzando la funzione `write` del modulo `File` per creare un nuovo file di testo e scrivere una semplice stringa al suo interno. Ora, se andiamo a controllare la nostra cartella di lavoro, dovremmo vedere il nuovo file "mionuovofile.txt" con il nostro testo al suo interno.

Ma cosa succede se vogliamo scrivere più di una riga di testo all'interno del nostro file? Possiamo farlo utilizzando la funzione `write_file` insieme al tipo di dati Elixir `IO.List`. Vediamo un esempio:

```elixir
# Creiamo una lista con le righe di testo che vogliamo scrivere
testi = ["Elixir è divertente da imparare!", "Sto scrivendo un file di testo.", "In alto i calici per il linguaggio funzionale!"]

# Scriviamo la lista nel file "miotest.txt"
File.write("miotest.txt", IO.inspect(testi))
```

In questo caso, stiamo utilizzando la funzione `IO.inspect` per stampare la lista di testo a schermo e poi passando il risultato alla funzione `write` del modulo `File`. Se andiamo a controllare il nostro nuovo file di testo, dovremmo vedere le tre righe di testo che abbiamo creato stampate una sotto l'altra.

## Approfondimento

Oltre a scrivere un file di testo, Elixir offre anche la possibilità di leggerne il contenuto con la funzione `read`. Ad esempio:

```elixir
# Leggiamo il contenuto del nostro file "miotest.txt"
File.read("miotest.txt")
```

Questo ci restituirà la lista di testo che avevamo salvato nel file. Inoltre, possiamo anche utilizzare la funzione `append` per aggiungere nuove righe di testo a un file esistente. Ad esempio:

```elixir
# Aggiungiamo una nuova riga di testo al nostro file "miotest.txt"
File.append("miotest.txt", "Elixir è un linguaggio versatile e potente!")
```

Se andiamo a controllare il nostro file, vedremo che la nuova riga di testo è stata aggiunta alla fine.

## Vedi Anche

- [Documentazione ufficiale di Elixir su File](https://hexdocs.pm/elixir/File.html)
- [Tutorial sull'uso di File in Elixir](https://medium.com/coding-technology/file-operations-with-elixir-d3a44dff1d09)