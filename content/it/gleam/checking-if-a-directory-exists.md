---
title:                "Verifica dell'esistenza di una directory"
date:                  2024-01-20T14:56:10.337139-07:00
html_title:           "Gleam: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
(Che Cosa & Perché?)

Controllare se una directory esiste significa verificare la presenza di un dato percorso nel file system. I programmatori lo fanno per evitare errori nei programmi quando provano ad accedere a file o directory che potrebbero non essere presenti.

## How to:
(Come fare:)

In Gleam, usiamo la libreria `gleam_stdlib` per interagire con il file system:

```gleam
import gleam/erlang/file
import gleam/result.{Result}

pub fn check_dir_exists(dir: String) -> Result(Bool, Nil) {
  file.dir_exists(dir)
}
```

Esempio di output quando verifichiamo una directory esistente:

```
> check_dir_exists("directory_esistente/")
Ok(True)
```

E quando non esiste:

```
> check_dir_exists("directory_inesistente/")
Ok(False)
```

## Deep Dive:
(Approfondimento)

Il controllo dell'esistenza di una directory è una pratica comune da decenni. Alternativamente, in altri linguaggi come Python, potresti usare `os.path.exists()`. In ambiente UNIX, il comando da terminale sarebbe `test -d`. 

In Gleam, che si basa sulla Erlang Virtual Machine (BEAM), l'operazione si appoggia agli strumenti di Erlang, quindi è robusta e affidabile. La funzione `file.dir_exists/1` ritorna un `Result`, che è un tipo che può rappresentare o un successo o un fallimento in modo sicuro, questo permette di gestire la presenza o meno della directory senza causare crash del programma.

## See Also:
(Vedi anche)

- Gleam standard library docs sul file handling: https://hexdocs.pm/gleam_stdlib/gleam/erlang/file/
- Erlang `file` module documentation: https://erlang.org/doc/man/file.html