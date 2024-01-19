---
title:                "Télécharger une page web"
html_title:           "Bash: Télécharger une page web"
simple_title:         "Télécharger une page web"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Télécharger une page Web signifie récupérer son contenu à travers le réseau. Les programmateurs le font pour analyser les données, collecter des informations ou sauvegarder du contenu pour un usage ultérieur.

## Comment faire :

Voici un exemple simple de la façon dont vous pouvez télécharger une page Web en utilisant la bibliothèque `reqwest` en Rust.

```Rust
use reqwest;
use std::io::Read;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let mut response = reqwest::get("http://example.com").await?;
    let mut body = String::new();
    response.read_to_string(&mut body);
    println!("{}", body);
    Ok(())
}
```

Dans cet exemple, nous demandons simplement à la bibliothèque `reqwest` de télécharger le contenu de "http://example.com", puis nous lisons la réponse dans une chaîne et l'affichons.

## Approfondissement

Lorsque nous parlons de télécharger une page Web, nous parlons en réalité de la requête HTTP GET. C'est une norme qui a été établie au début du web par le protocole HTTP, et qui est utilisée pour récupérer des informations à partir d'un serveur.

Il existe d'autres moyens d'obtenir le contenu d'une page Web. Un exemple populaire serait le `Web Scraping`, qui ne se contente pas de télécharger la page, mais tente également d'interpréter le HTML et de collecter spécifiquement les données qui intéressent le programmeur.

Au niveau de l'implémentation, notre exemple est assez simpliste. En réalité, le téléchargement d'une page Web peut impliquer une gestion plus complexe des erreurs, le respect du taux limite du serveur, l'exécution de JavaScript sur la page, ou encore le contournement des mesures de protection contre les robots.

## Voir aussi 

Pour plus d'informations, consultez les liens suivants :

1. Documentation `reqwest` : https://docs.rs/reqwest/
2. Wikipédia sur la Requête HTTP GET : https://fr.wikipedia.org/wiki/Hypertext_Transfer_Protocol#Requête_GET
3. Tutoriel Rust (en anglais) : https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-misconceptions.html