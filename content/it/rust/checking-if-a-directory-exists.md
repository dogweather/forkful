---
title:                "Verifica se una directory esiste"
html_title:           "Lua: Verifica se una directory esiste"
simple_title:         "Verifica se una directory esiste"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Controlla se una directory esiste in Rust

## Cosa & Perchè?

Controllare se una directory esiste significa verificare la presenza fisica di una cartella specifica nel filesystem del tuo PC. Questo viene spesso fatto dai programmatori per evitare errori quando tali cartelle sono necessarie per leggere o scrivere file.

## Come Fare:

Verifichiamo la presenza di una directory utilizzando la funzione `exists()` dal modulo `std::path`.

```Rust
use std::path::Path;

fn main() {
    let path = Path::new("/percorso/della/cartella");

    if path.exists() {
        println!("La directory esiste.");
    } else {
        println!("La directory non esiste.");
    }
}
```
Se la directory esiste, vedrai l'output "La directory esiste.", altrimenti "La directory non esiste.".

Ora, creiamo una directory se non esiste.

```Rust
use std::fs;

fn main() {
    let path = "/percorso/della/cartella";
    
    if !Path::new(&path).exists() {
        fs::create_dir_all(&path).unwrap();
    }
}
```
In questo esempio, utilizziamo la funzione `create_dir_all` di `std::fs`per creare la directory e le sue sottodirectory (se necessarie).

## Approfondimento

Nel corso degli anni, il controllo della presenza di una directory è stato gestito in diversi modi nei vari linguaggi di programmazione. In Rust, le funzioni del modulo `std::path` e `std::fs` rendono questo compito semplice ed efficiente.

Un'alternativa a `exists()` è utilizzare il metodo `metadata()`. Tuttavia `exists()` è più leggibile ed esplicita sul suo scopo, quindi è generalmente preferita.

Sul lato implementazione, `exists()` chiama internamente la funzione `metadata().is_ok()`. Non solo controlla se la directory esiste, ma anche se i metadati possono essere recuperati.

Inoltro, vale la pena ricordare che c'è una sottile differenza tra 'verificare se un path esiste' e 'verificare se un path è una directory'. `Path::exists()`, potrebbe restituire true anche se il path non è una directory, ma un file. Per verificare se il path è effettivamente una directory, si dovrebbe usare `Path::is_dir()`.

## Vedere Anche

Per ulteriori dettagli, dai un'occhiata a queste risorse:

- Documentazione Rust sul modulo `std::path`: https://doc.rust-lang.org/std/path/
- Documentazione Rust sul modulo `std::fs`: https://doc.rust-lang.org/std/fs/
- Forum della community Rust con discussioni relative: https://users.rust-lang.org