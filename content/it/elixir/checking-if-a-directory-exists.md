---
title:                "Elixir: Verifica l'esistenza di una directory"
simple_title:         "Verifica l'esistenza di una directory"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché controllare l'esistenza di una directory in Elixir?

Controllare l'esistenza di una directory è un'operazione comune durante lo sviluppo di applicazioni in Elixir. Spesso abbiamo bisogno di verificare se una directory è già presente prima di crearne una nuova o di effettuare un'operazione su di essa. In questo articolo, scopriremo come controllare se una directory esiste utilizzando il potente linguaggio di programmazione Elixir.

## Come fare

In Elixir, possiamo utilizzare la funzione `File.dir?/1` per verificare se una directory esiste o meno. Questa funzione prende come argomento il percorso della directory e restituisce `true` se la directory esiste, `false` altrimenti. Vediamo un esempio di codice che utilizza questa funzione:

```Elixir
# Utilizziamo la funzione File.dir?/1 per verificare se la directory "images" esiste
File.dir?("images")
# Output: true
```

Se la directory non esiste, la funzione restituirà `false`:

```Elixir
# Verifichiamo se la directory "documents" esiste
File.dir?("documents")
# Output: false
```

Puoi anche utilizzare questa funzione con percorsi assoluti o relativi alla directory corrente:

```Elixir
# Utilizziamo la funzione File.dir?/1 con un percorso assoluto
File.dir?("/home/user/documents")
# Output: true

# Utilizziamo la funzione File.dir?/1 con un percorso relativo
File.dir?("docs")
# Output: true
```

## Approfondimento

Detto questo, è importante sottolineare che la funzione `File.dir?/1` non garantisce che la directory non verrà eliminata o spostata in un secondo momento. Pertanto, è sempre consigliabile gestire eventuali errori durante l'accesso alle directory.

Inoltre, se vuoi controllare l'esistenza di una directory e contemporaneamente ottenere ulteriori informazioni su di essa (ad esempio, le sue dimensioni o le autorizzazioni di accesso), puoi utilizzare la funzione `File.stat/1`:

```Elixir
# Utilizziamo la funzione File.stat/1 per ottenere informazioni sulla directory "images"
File.stat("images")
# Output: {:ok, %File.Stat{mode: 33261, mtime: 1546253840, size: 4096, type: :directory, atime: 1546253840, ctime: 1546253840}}

# Stampiamo solo il tipo della directory
{:ok, stat} = File.stat("images")
stat.type
# Output: :directory
```

## Vedi anche

- [Documentazione di Elixir sulla funzione File.dir?/1](https://hexdocs.pm/elixir/File.html#dir?/1)
- [Come creare una directory in Elixir](https://example-link.com)
- [Tutorial su come gestire gli errori in Elixir](https://example-link.com)