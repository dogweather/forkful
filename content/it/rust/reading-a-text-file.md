---
title:                "Rust: Leggere un file di testo"
simple_title:         "Leggere un file di testo"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Leggere un file di testo è un'operazione fondamentale nella programmazione, soprattutto quando si lavora con dati esterni. In questo articolo, scopriremo come leggere un file di testo utilizzando il linguaggio di programmazione Rust.

## Come Fare

Per prima cosa, dobbiamo importare la libreria standard di Rust che ci permetterà di gestire i file di testo. Per fare ciò, inseriamo la seguente riga all'inizio del nostro codice:

```Rust
use std::fs::File;
```

Successivamente, dobbiamo aprire il file che desideriamo leggere utilizzando il metodo `File::open()`, passando come argomento il nome del file. Ad esempio, se il nostro file si chiama "dati.txt", il codice sarà il seguente:

```Rust
let file = File::open("dati.txt");
```

Una volta aperto il file, possiamo leggerne il contenuto utilizzando il metodo `read_to_string()`, come mostrato nell'esempio seguente:

```Rust
let content = file.read_to_string();
```

Infine, possiamo stampare il contenuto del file utilizzando il metodo `println!()` come nell'esempio seguente:

```Rust
println!("Contenuto del file: {}", content);
```

## Deep Dive

Ora che sappiamo come leggere un file di testo in Rust, è importante sottolineare alcune considerazioni. In primo luogo, dobbiamo assicurarci che il file che vogliamo leggere si trovi nella stessa cartella del nostro programma rust, in caso contrario sarà necessario specificare il percorso completo del file. Inoltre, è importante tenere conto che quando si lavora con file di grandi dimensioni, il metodo `read_to_string()` potrebbe causare problemi di prestazioni. In questi casi, è consigliato utilizzare il metodo `read()` che legge il contenuto del file come un array di byte.

## Vedi Anche

- [Documentazione ufficiale sulla gestione dei file in Rust](https://doc.rust-lang.org/std/fs/struct.File.html)
- [Rust Cookbook sui file di testo](https://rust-lang-nursery.github.io/rust-cookbook/text/io.html)
- [Tutorial su come leggere un file di testo in Rust](https://www.tutorialspoint.com/read-a-file-line-by-line-in-rust)