---
title:                "Gleam: Verifica se una directory esiste"
simple_title:         "Verifica se una directory esiste"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Controllare se una directory esiste può essere utile per verificare l'esistenza di un percorso o per gestire eccezioni nel caso in cui una directory non sia accessibile.

## Come fare

Per controllare se una directory esiste in Gleam, possiamo utilizzare la libreria standard `gleam_io` e la sua funzione `Dir.exists`:

```Gleam
import gleam/io
result = gleam/io.Dir.exists("/home/user/documents")

// output: true
```

Se la directory esiste, il risultato sarà `true`. In caso contrario, il risultato sarà `false`.

## Approfondimento

La funzione `Dir.exists` utilizza l'operatore `file_info` della libreria standard `gleam_os` per ottenere informazioni sui file e sulle directory. Utilizzando l'annotazione del tipo di ritorno `:?FileInfo`, possiamo ottenere informazioni aggiuntive sulla directory, come la dimensione, il timestamp dell'ultima modifica e i permessi di accesso.

```Gleam
import gleam/io
import gleam/os

fn check_directory(path) {
  case gleam/io.Dir.exists(path): {
    true -> true
    false -> false
  }
}

test "check if directory exists" {
  expect(check_directory("/home/user/documents")) === true
}

// ?FileInfo { size: 1234, modified: 2021-08-12T12:00:00Z, permissions: Perm(0755) }
```

## Vedi anche

- [Documentazione della libreria standard gleam_io](https://gleam.run/docs/stdlib-io)
- [Documentazione della libreria standard gleam_os](https://gleam.run/docs/stdlib-os)
- [Esempio di utilizzo della funzione `Dir.exists`](https://github.com/gleam-lang/gleam/blob/master/examples/io/files.exs)