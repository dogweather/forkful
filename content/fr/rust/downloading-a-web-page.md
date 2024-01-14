---
title:                "Rust: Le téléchargement d'une page web"
simple_title:         "Le téléchargement d'une page web"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur curieux à la recherche d'un nouveau langage de programmation polyvalent et performant, alors Rust peut être le langage dont vous avez besoin. Avec des fonctionnalités de sécurité avancées, de la concurrence native, et une compilation rapide, Rust est idéal pour une variété de projets, y compris le téléchargement de pages web. Dans cet article, nous allons plonger dans la manière de télécharger une page web à l'aide de Rust.

## Comment faire

Pour télécharger une page web en utilisant Rust, nous allons utiliser la bibliothèque standard "reqwest". Vous pouvez l'ajouter à votre projet en l'incluant dans le fichier "Cargo.toml" :

```Rust
[dependencies]
reqwest = "0.11"
```

Ensuite, vous pouvez utiliser ces lignes de code pour télécharger une page web et récupérer son contenu :

```Rust
use reqwest::get;

fn main() {
    let response = get("https://www.example.com")
        .unwrap()
        .text()
        .unwrap();
    println!("{}", response);
}
```

Le code ci-dessus importe la fonction `get` de la bibliothèque "reqwest" et utilise sa méthode `text()` pour récupérer le contenu de la page web sous forme de chaîne de caractères. Ensuite, nous imprimons simplement le contenu à la console.

Si vous souhaitez enregistrer le contenu dans un fichier, vous pouvez utiliser la méthode `write` de la bibliothèque standard de Rust :

```Rust
use reqwest::get;
use std::io::Write;
use std::fs::File;

fn main() {
    let mut response = get("https://www.example.com")
        .unwrap()
        .text()
        .unwrap();
    let mut file = File::create("example.html").unwrap();
    file.write_all(response.as_bytes()).unwrap();
}
```

Ce code télécharge la page web et enregistre son contenu dans un fichier "example.html".

## Deep Dive

Si vous souhaitez aller plus en profondeur, vous pouvez également personnaliser votre requête en spécifiant des headers, des paramètres et des cookies. Par exemple, voici comment ajouter un header et un paramètre à votre requête :

```Rust
use reqwest::Client;
use std::collections::HashMap;

fn main() {
    let client = Client::new();
    let mut headers = HashMap::new();
    headers.insert("User-Agent", "My Rust Downloader");
    let params = [("lang", "rust"), ("category", "web")];
    let response = client.get("https://www.example.com")
        .headers(headers)
        .query(&params)
        .send()
        .unwrap()
        .text()
        .unwrap();
    println!("{}", response);
}
```

Ce code utilise la classe `Client` de la bibliothèque "reqwest" pour créer une requête personnalisée avec un header et des paramètres spécifiés. Vous pouvez également ajouter des cookies en utilisant la méthode `cookie` et en passant un cookie en tant que paramètre. Consultez la documentation de la bibliothèque pour plus d'informations sur les autres fonctionnalités disponibles.

## Voir aussi

- La documentation officielle de la bibliothèque "reqwest": https://docs.rs/reqwest/0.11.3/reqwest/index.html
- Un tutoriel complet sur Rust pour les débutants : https://tourofrust.com/00_fr.html
- Un cours gratuit sur Rust sur Udemy : https://www.udemy.com/course/formation-complete-rust/learn/lecture411274?start=60#overview