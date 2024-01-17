---
title:                "Analyse de l'html"
html_title:           "Rust: Analyse de l'html"
simple_title:         "Analyse de l'html"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/parsing-html.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi le faire?

Parser le HTML est le processus de traiter et d'analyser du texte HTML pour en extraire des informations ou le transformer en une autre forme. Les programmeurs le font souvent pour extraire des données spécifiques d'un site web ou pour créer des applications qui nécessitent une compréhension du contenu HTML.

# Comment faire :

Voici un exemple simple en Rust pour montrer comment parser du HTML :

```Rust
use scraper::{Html, Selector};

fn main() {
    // Télécharge le contenu HTML de la page web
    let html = reqwest::blocking::get("https://www.example.com").unwrap().text().unwrap();
    // Convertit le HTML en une structure que nous pouvons manipuler
    let document = Html::parse_document(&html);
    // Sélectionne tous les liens de la page
    let links = Selector::parse("a").unwrap();
    for link in document.select(&links) {
        // Affiche l'URL de chaque lien
        println!("{}", link.value().attr("href").unwrap());
    }
}
```

Sortie :

```
https://www.example.com/about
https://www.example.com/contact
https://www.example.com/blog
```

# Zoom sur :

Parsing HTML remonte à l'époque où le World Wide Web a été créé. Les développeurs ont rapidement réalisé que pour créer des applications basées sur le web, ils devaient pouvoir extraire des données à partir de pages web. Aujourd'hui, il existe de nombreux outils pour parser du HTML en Rust, tels que ```scraper```, ```html5ever```, et ```select```. Alternativement, certains programmeurs peuvent préférer utiliser des langages comme Python ou JavaScript pour le parsing HTML.

# Voir aussi :

- [Documentation Rust de scraper](https://docs.rs/scraper)
- [Exemples de parsing HTML en Rust](https://github.com/greyblake/whatlang-rs/blob/master/examples/markdown/src/main.rs)
- [Article Wikipedia sur le parsing HTML](https://fr.wikipedia.org/wiki/HTML#Parsing)