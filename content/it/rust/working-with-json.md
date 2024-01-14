---
title:                "Rust: Programmare con json"
simple_title:         "Programmare con json"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/working-with-json.md"
---

{{< edit_this_page >}}

# Perché lavorare con JSON in Rust?

Sebbene Rust sia un linguaggio di programmazione relativamente nuovo, sta diventando sempre più popolare per la sua sicurezza e prestazioni. La gestione dei formati di dati come JSON è fondamentale per molte applicazioni, quindi imparare a lavorare con esso in Rust può essere estremamente utile.

## Come fare

In Rust, il modo più comune per lavorare con JSON è utilizzare una libreria esterna chiamata Serde. Per utilizzarla, è necessario aggiungere la seguente dipendenza al file Cargo.toml del progetto:

```Rust
[dependencies]
serde = "1.0"
serde_json = "1.0"
```

Una volta aggiunta la dipendenza, è possibile iniziare a lavorare con JSON nel proprio codice. Ad esempio, per convertire un oggetto JSON in una struttura dati di Rust, si può utilizzare il seguente codice:

```Rust
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct User {
    name: String,
    age: u8,
}

fn main() {
    let json_string = r#"
        {
            "name": "Marco",
            "age": 27
        }
    "#;

    let user: User = serde_json::from_str(json_string).unwrap();

    println!("Name: {}", user.name);
    println!("Age: {}", user.age);
}
```

L'output di questo codice sarà:

```
Name: Marco
Age: 27
```

Invece, per convertire una struttura dati di Rust in un oggetto JSON, si può utilizzare il seguente codice:

```Rust
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct User {
    name: String,
    age: u8,
}

fn main() {
    let user = User { name: "Sara".to_string(), age: 30 };

    let json_string = serde_json::to_string(&user).unwrap();

    println!("{}", json_string);
}
```

L'output di questo codice sarà:

```
{"name":"Sara","age":30}
```

## Approfondimento

Mentre Serde semplifica notevolmente il lavoro con JSON in Rust, è importante comprendere come funziona effettivamente la conversione dei dati in formato JSON. Ad esempio, quando si utilizza `serde_json::to_string()` per convertire una struttura dati in JSON, Serde in realtà utilizza la funzione `Serialize` per tradurre i dati in un formato che può essere facilmente convertito in JSON. Allo stesso modo, quando si utilizza `serde_json::from_str()` per convertire una stringa JSON in una struttura dati, Serde utilizza la funzione `Deserialize` per trasformare i dati JSON in un formato compatibile con Rust.

Inoltre, Serde offre diverse configurazioni per gestire casi particolari come nomi di campi diversi tra la struttura dati e l'oggetto JSON. È possibile leggere di più su queste configurazioni nella documentazione ufficiale di Serde.

## Vedi anche

- Documentazione ufficiale di Serde: https://serde.rs/
- Crash course su Serde: https://blog.logrocket.com/serializing-and-deserializing-rust-data-types-with-serde/
- Esempio pratico di utilizzo di Serde per lavorare con API in JSON: https://dev.to/peregrinius/working-with-json-api-in-rust-5ckj