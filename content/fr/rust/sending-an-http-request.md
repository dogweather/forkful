---
title:                "Envoyer une demande http"
html_title:           "Rust: Envoyer une demande http"
simple_title:         "Envoyer une demande http"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Pourquoi

Tu as probablement entendu parler de l'envoi de requêtes HTTP, mais tu ne sais pas exactement en quoi cela consiste ou pourquoi tu devrais le faire. En bref, envoyer une requête HTTP te permet d'envoyer des informations à un serveur distant et de recevoir des réponses en retour. Tu peux penser à cela comme à l'envoi d'une lettre à un ami et en attendant une réponse. Cela peut être très utile pour communiquer avec des API et récupérer des données à utiliser dans tes projets.

## Comment faire

Pour envoyer une requête HTTP en Rust, tu auras besoin de la bibliothèque standard `reqwest`. Tout d'abord, tu devras l'ajouter à ton fichier `Cargo.toml` :

```
[dependencies]
reqwest = "0.11"
```

Ensuite, tu pourras importer la bibliothèque dans ton code :

```Rust
use reqwest;
```

Pour envoyer une requête GET, tu peux utiliser la fonction `get()` en passant l'URL en tant que paramètre :

```Rust
let response = reqwest::get("https://www.example.com").await?;
```

Tu peux également spécifier des paramètres dans l'URL, comme dans cet exemple où nous passons un ID pour récupérer des données spécifiques :

```Rust
let response = reqwest::get("https://www.example.com/users?id=123").await?;
```

Pour envoyer une requête POST, tu dois spécifier le corps de la requête dans un `HashMap`, puis l'envoyer avec la fonction `post()`. Tu peux également ajouter des en-têtes ou des paramètres à la requête si nécessaire :

```Rust
let mut params = HashMap::new();
params.insert("username", "john");
params.insert("password", "secret");

let response = reqwest::Client::new()
    .post("https://www.example.com/login")
    .headers(headers)
    .form(&params)
    .send()
    .await?;
```

Tu peux maintenant utiliser la réponse pour accéder aux en-têtes, au corps et aux autres informations associées :

```Rust
println!("Status code: {}", response.status());
let headers = response.headers();

let body = response.text().await?;
println!("Body: {}", body);
```

Et voilà ! Tu viens d'envoyer ta première requête HTTP en Rust.

## Plongeon plus profond

Si tu veux en savoir plus sur l'envoi de requêtes HTTP en Rust, tu peux consulter la documentation officielle de `reqwest` pour trouver d'autres méthodes et options. Tu pourrais également te pencher sur l'implémentation de ces bibliothèques pour mieux comprendre le fonctionnement sous-jacent.

## Voir aussi

- Documentation officielle de `reqwest`: https://docs.rs/reqwest/0.11.4/reqwest/
- Tutoriel sur les requêtes HTTP en Rust: https://gill.net.in/posts/rust/2019/02/17/rust-http-vs-hyper-vs-hyperium/