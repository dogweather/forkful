---
title:                "Lecture d'un fichier texte"
aliases:
- /fr/rust/reading-a-text-file/
date:                  2024-01-20T17:55:00.330171-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lecture d'un fichier texte"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
En Rust, lire un fichier texte, c'est collecter son contenu pour l'utiliser dans votre programme. On le fait souvent pour traiter des données en batch, charger des configurations, où simplement lire des instructions.

## How to:
Pour lire un fichier texte, on utilise principalement `std::fs` et `std::io`. Voilà un exemple simple :

```rust
use std::fs;
use std::io::{self, Read};

fn main() -> io::Result<()> {
    let mut contenu = String::new();
    
    // Ouvre le fichier et le lit
    fs::File::open("exemple.txt")?.read_to_string(&mut contenu)?;
    
    println!("Contenu du fichier:\n{}", contenu);
    
    Ok(())
}
```

Si `exemple.txt` contient "Bonjour, Rust!", la sortie sera :

```
Contenu du fichier:
Bonjour, Rust!
```

## Deep Dive
Lire des fichiers est crucial depuis l'aube de l'informatique. En Rust, on privilégie la gestion d'erreur avec `Result<T, E>`. Cela force à réfléchir aux erreurs durant la programmation, évitant ainsi des surprises déplaisantes à l'exécution.

D'autres moyens existent, tel que `std::fs::read_to_string`, qui fait le même travail en moins de lignes :

```rust
use std::fs;

fn main() -> Result<(), std::io::Error> {
    let contenu = fs::read_to_string("exemple.txt")?;
    println!("Contenu du fichier:\n{}", contenu);
    Ok(())
}
```

Pour les gros fichiers, on lit ligne par ligne avec `BufRead` pour économiser de la mémoire :

```rust
use std::fs::File;
use std::io::{self, BufRead};

fn main() -> io::Result<()> {
    let fichier = File::open("exemple.txt")?;
    let lecteur = io::BufReader::new(fichier);

    for ligne in lecteur.lines() {
        println!("{}", ligne?);
    }

    Ok(())
}
```

## See Also
Pour approfondir, checkez les liens suivants :

- [Rust by Example - File I/O](https://doc.rust-lang.org/rust-by-example/std_misc/file.html)
- [Rust `std::fs` Module Documentation](https://doc.rust-lang.org/std/fs/index.html)
