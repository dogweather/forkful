---
title:                "Creazione di un file temporaneo"
html_title:           "Rust: Creazione di un file temporaneo"
simple_title:         "Creazione di un file temporaneo"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

#
## Perché
Creare un file temporaneo è un'operazione comune nella programmazione di Rust e può essere utile per molteplici ragioni, come il salvataggio temporaneo dei dati, la creazione di file di log o il testing di funzionalità dell'applicazione.

## Come fare
Per creare un file temporaneo in Rust, occorre utilizzare la libreria standard ```std::fs``` e il metodo ```tempfile()```.
```
use std::fs::{self, File};
use std::io::Write;

fn main() {
  // Creazione del file temporaneo
  let mut temp_file = File::tempfile().expect("Impossibile creare il file temporaneo.");

  // Scrittura di dati nel file
  write!(temp_file, "Questo è un file temporaneo.").expect("Impossibile scrivere nel file.");

  // Salvataggio dei dati nel file
  temp_file.flush().expect("Impossibile salvare i dati nel file.");

  // Visualizzazione del percorso del file temporaneo
  println!("Il file temporaneo si trova in: {}", temp_file.path().display());

  // Eliminazione del file temporaneo
  fs::remove_file(temp_file.path()).expect("Impossibile eliminare il file temporaneo.");
}
```
L'output del codice sopra riportato dovrebbe essere qualcosa del genere:
```
Il file temporaneo si trova in: /tmp/tmp.K3o0sMaZPb
```

## Approfondimenti
Durante la creazione di un file temporaneo, il sistema operativo assegna automaticamente un nome e un percorso al file, che verranno restituiti dal metodo ```tempfile()```. Inoltre, è possibile specificare un prefisso per il nome del file utilizzando il metodo ```tempfile_named()```. In entrambi i casi, il file temporaneo viene automaticamente eliminato quando il programma termina, ma è possibile mantenerlo attivo utilizzando il metodo ```keep()```.

## Vedi anche
- [Documentazione ufficiale sulla creazione di file temporanei in Rust](https://doc.rust-lang.org/std/fs/struct.File.html#method.tempfile)
- [Tutorial su come lavorare con file in Rust](https://www.steadylearner.com/blog/read/How-to-use-Rust-Files-Easily-and-effectively)
- [Esempi di codice per la gestione dei file in Rust](https://github.com/rust-lang/rust-cookbook/tree/master/src/file)