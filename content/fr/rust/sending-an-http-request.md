---
title:                "Envoyer une requête http"
html_title:           "Bash: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Envoyer une requête HTTP consiste à demander des informations à un serveur Web. Les programmeurs le font généralement pour obtenir ou manipuler des données depuis/vers un serveur distant.

## Comment faire:

Voici comment on peut faire cela en Rust:

```Rust
// Importer le crate
extern crate reqwest;

// Fonction asynchrone pour envoyer une requête GET
#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let response = reqwest::get("https://www.google.com").await?;

    println!("{}", response.status());

    Ok(())
}
```

L'exécution de ce code donnera une sortie similaire à :

```Rust
200 OK
```

## Plus d'informations:

L'envoi de requêtes HTTP est une pratique courante depuis l'avènement du Web dans les années 90. Bien qu'il existe plusieurs alternatives pour envoyer une requête HTTP, comme Curl ou Python Requests, Rust offre toutefois des avantages en termes de sécurité et de rapidité.

Une implémentation en Rust pourrait utiliser le crate `reqwest`, qui se charge de la majorité du travail en coulisses. Le crate gère les requêtes HTTP de manière asynchrone, ce qui signifie que votre programme n'a pas à rester inactif pendant que le serveur répond à la requête.

## Voir aussi:

Pour plus d'information sur l'envoi de requêtes HTTP en Rust, vous pouvez consulter ces ressources :

- [Documentation sur le crate `reqwest`](https://docs.rs/reqwest/0.11.0/reqwest/index.html)