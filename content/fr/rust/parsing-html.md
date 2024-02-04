---
title:                "Analyse Syntaxique du HTML"
date:                  2024-02-03T19:12:56.048147-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analyse Syntaxique du HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Analyser du HTML en Rust consiste à extraire des données de documents HTML, ce qui est essentiel pour le web scraping, l'extraction de données ou la création de web crawlers. Les programmeurs font cela pour automatiser la collecte d'informations sur le web, analyser le contenu web ou migrer du contenu d'une plateforme à une autre.

## Comment faire :

Pour analyser du HTML en Rust, vous utiliserez souvent le crate `scraper`, qui fournit une interface de haut niveau pour parcourir et manipuler des documents HTML.

Premièrement, ajoutez `scraper` à votre `Cargo.toml` :

```toml
[dependencies]
scraper = "0.12.0"
```

Ensuite, voici un exemple simple qui extrait toutes les URLs de lien d'une chaîne HTML donnée :

```rust
extern crate scraper;

use scraper::{Html, Selector};

fn main() {
    let html = r#"
    <html>
    <body>
        <a href="http://example.com/1">Lien 1</a>
        <a href="http://example.com/2">Lien 2</a>
    </body>
    </html>
    "#;

    let document = Html::parse_document(html);
    let selector = Selector::parse("a").unwrap();

    for element in document.select(&selector) {
        let link = element.value().attr("href").unwrap();
        println!("Lien trouvé : {}", link);
    }
}
```

Sortie :

```
Lien trouvé : http://example.com/1
Lien trouvé : http://example.com/2
```

Dans cet exemple, nous analysons un document HTML simple pour trouver tous les éléments `<a>` et extraire leurs attributs `href`, imprimant effectivement les URLs de tous les liens dans le document. La bibliothèque `scraper` simplifie l'analyse HTML et la sélection d'éléments spécifiques en utilisant les sélecteurs CSS, la rendant incontournable pour les tâches de web scraping en Rust.
