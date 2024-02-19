---
aliases:
- /fr/rust/sending-an-http-request/
date: 2024-01-20 18:00:46.112683-07:00
description: "Envoyer une requ\xEAte HTTP, c'est demander des donn\xE9es \xE0 un serveur\
  \ web. Les programmeurs font \xE7a pour int\xE9ragir avec des APIs, r\xE9cup\xE9\
  rer des fichiers, ou\u2026"
lastmod: 2024-02-18 23:09:08.525403
model: gpt-4-1106-preview
summary: "Envoyer une requ\xEAte HTTP, c'est demander des donn\xE9es \xE0 un serveur\
  \ web. Les programmeurs font \xE7a pour int\xE9ragir avec des APIs, r\xE9cup\xE9\
  rer des fichiers, ou\u2026"
title: "Envoi d'une requ\xEAte HTTP"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)

Envoyer une requête HTTP, c'est demander des données à un serveur web. Les programmeurs font ça pour intéragir avec des APIs, récupérer des fichiers, ou soumettre des informations.

## How to: (Comment faire :)

On va utiliser `reqwest`, une bibliothèque Rust populaire. Ajoutez d'abord `reqwest` à votre `Cargo.toml`:

```toml
[dependencies]
reqwest = "0.11"
tokio = { version = "1", features = ["full"] }
```

Puis, envoyez une requête GET simple :

```rust
use reqwest;
use tokio;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let response = reqwest::get("http://www.example.com").await?;
    
    println!("Status: {}", response.status());
    println!("Headers:\n{:#?}", response.headers());

    let body = response.text().await?;
    println!("Body:\n{}", body);

    Ok(())
}
```

Sortie attendue (en fonction du contenu actuel de example.com) :

```
Status: 200 OK
Headers:
{
    "content-type": "text/html; charset=UTF-8",
    // ...
}
Body:
<!doctype html>
...
```

## Deep Dive (Plongée en profondeur)

L'envoi de requêtes HTTP est un concept vieux comme le web lui-même. Avant `reqwest`, Rust utilisait `hyper`, toujours utilisé en sous-main par `reqwest` pour le traitement HTTP bas-niveau. Des alternatives à `reqwest` incluent `hyper` pour plus de contrôle et `surf` pour un environnement async runtime-agnostique. L'implémentation consiste à établir une connexion TCP avec le serveur, envoyer une requête formatée selon la spécification HTTP et interpréter la réponse.

## See Also (Voir aussi)

- Documentation Reqwest : https://docs.rs/reqwest/
- Asynchronous Programming in Rust : https://rust-lang.github.io/async-book/
- Hyper, un client HTTP bas niveau : https://hyper.rs/
- Surf, un autre client HTTP en Rust : https://github.com/http-rs/surf
