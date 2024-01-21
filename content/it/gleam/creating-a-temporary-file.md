---
title:                "Creazione di un file temporaneo"
date:                  2024-01-20T17:40:21.946074-07:00
model:                 gpt-4-1106-preview
simple_title:         "Creazione di un file temporaneo"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?
Creare un file temporaneo significa fare un file che è destinato a essere usato per un breve periodo di tempo. I programmatori lo fanno per non alterare i dati permanenti o per lavorare con dati che durano il tempo di una sessione.

## How to:
Per ora, usiamo la libreria standard di Erlang, visto che Gleam non ha una funzione built-in per la creazione di file temporanei.

```gleam
import gleam/erlang
import gleam/io

pub fn create_temp_file() -> Result(BitString, Nil) {
  // Creiamo un file temporaneo con un nome unico
  let temp_path = os.tmpnam() 
                      |> result.unwrap("Failed to get a unique temp file path")

  // Scriviamo qualcosa nel file temporaneo come esempio
  erlang.write_file(temp_path, "Contenuto temporaneo", [])

  // Leggiamo quello che abbiamo scritto
  case erlang.read_file(temp_path) {
    Ok(contents) ->
      io.debug(contents)
      Ok(contents)
    
    Error(_) ->
      Error(Nil)
  }
}

// Quando esegui `create_temp_file()`, avrai una stampa con il contenuto del file temporaneo.
```

## Deep Dive
In Erlang (e quindi in Gleam tramite FFI), si usa spesso `os:tempnam/0` per ottenere un percorso unico per un file temporaneo. Tuttavia, questo metodo è deprecato in molti sistemi operativi per problemi di sicurezza; un attaccante potrebbe prevedere il nome del file temporaneo e crearne uno prima che il vostro codice lo faccia. In alternativa, si può usare `os:mktemp/1` che crea il file direttamente evitando race conditions. 

Però, fate attenzione: entrambe le funzioni sono lowest common denominator APIs disponibili in diverse piattaforme. Quando la libreria standard Gleam avrà un proprio modo per gestire i file temporanei, sarà una scelta migliore per evitare problemi di compatibilità e sicurezza.

## See Also
- Documentazione Erlang `os` module: [Erlang os module](http://erlang.org/doc/man/os.html)
- Approfondimento sulla sicurezza dei file temporanei: [OWASP Secure File Handling](https://owasp.org/www-community/vulnerabilities/Insecure_Temporary_File)