---
title:                "Rust: Verifica dell'esistenza di una directory"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché
Se sei un programmatore che utilizza il linguaggio Rust, probabilmente stai cercando un modo per determinare se una directory esiste o meno all'interno del tuo codice. In questo articolo, ti mostreremo come farlo utilizzando alcune funzioni standard di Rust.

## Come fare
Per iniziare, devi importare il pacchetto ```std::fs```, che contiene le funzioni necessarie per gestire i file e le directory. Successivamente, puoi utilizzare la funzione ```fs::metadata()``` per ottenere informazioni sulla directory che stai cercando. Questa funzione restituirà un ```std::fs::Metadata``` object che contiene informazioni come il proprietario, i permessi e la data di modifica.

```
use std::fs;

match fs::metadata("directory") {
    Ok(metadata) => {
        //La directory esiste
        println!("La directory esiste!");
    },
    Err(error) => {
        //La directory non esiste
        println!("La directory non esiste!");
    }
}
```

In questo esempio, abbiamo utilizzato un costrutto ```match``` per gestire il risultato della funzione ```fs::metadata()```. Se la directory esiste, il codice all'interno del blocco ```Ok``` verrà eseguito. Altrimenti, se si verifica un errore, il codice all'interno del blocco ```Err``` verrà eseguito.

Una volta che hai determinato che la directory esiste, puoi utilizzare la funzione ```fs::read_dir()``` per leggere il contenuto della directory. Questa funzione restituirà un ```std::fs::ReadDir``` object che può essere iterato per ottenere i nomi dei file e delle directory all'interno della directory specificata.

```
use std::fs;

for entry in fs::read_dir("directory").unwrap() {
    let dir = entry.unwrap();
    println!("{}", dir.path().display());
}
```

In questo secondo esempio, abbiamo utilizzato un ciclo ```for``` per iterare su tutti gli elementi all'interno della directory. Utilizzando il metodo ```path()``` sull'oggetto restituito da ```ReadDir```, si può ottenere il percorso completo di ogni file o directory all'interno della directory specificata.

## Deep Dive
Oltre alle funzioni menzionate sopra, esistono anche altre opzioni per gestire la verifica dell'esistenza di una directory in Rust. Ad esempio, puoi utilizzare la funzione ```fs::File::create()``` per creare una nuova directory se questa non esiste già.

```
use std::fs::File;

let result = File::create("directory");

match result {
    Ok(_) => println!("La directory è stata creata con successo!"),
    Err(error) => println!("Impossibile creare la directory: {}", error),
}
```

Come si può vedere, questa funzione restituirà un ```std::io:Result``` object che dovrebbe essere gestito utilizzando un costrutto ```match```. Utilizzando questa opzione, è possibile creare una directory solo se non esiste già, evitando di sovrascrivere qualcosa di importante.

## Vedi anche
- [Documentazione ufficiale di Rust per il modulo ```std::fs```](https://doc.rust-lang.org/std/fs/)
- [Esempi di codice per la gestione dei file e delle directory in Rust](https://gist.github.com/jmatraszek/670c515dbe8090ed7a9731f4389f5386)