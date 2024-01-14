---
title:                "Rust: Envoyer une demande http avec une authentification de base"
simple_title:         "Envoyer une demande http avec une authentification de base"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous développez des applications web en utilisant Rust, vous pourriez avoir besoin d'envoyer des requêtes HTTP avec une authentification de base. Cela peut être nécessaire pour sécuriser l'accès à certaines ressources ou pour implémenter des fonctionnalités telles que l'authentification des utilisateurs.

## Comment faire

Pour envoyer une requête HTTP avec une authentification de base en Rust, vous pouvez utiliser la bibliothèque standard `reqwest`. Tout d'abord, vous devrez ajouter cette dépendance à votre `Cargo.toml` :

```Rust
[dependencies]
reqwest = { version = "0.11.0", features = ["json"] }
```

Ensuite, vous pourrez importer la bibliothèque et créer une nouvelle requête HTTP avec l'option d'authentification de base :

```Rust
use reqwest::header::{Authorization, Basic};
use reqwest::Client;

// Création d'un client HTTP
let client = Client::new();

// Création d'une requête avec une authentification de base
let req = client.get("https://exemple.com/ressource")
    .header(Authorization(Basic{
        username: "utilisateur".to_string(),
        password: Some("motdepasse".to_string())
}));

// Envoi de la requête et récupération de la réponse
let res = req.send()?;

// Affichage du corps de la réponse
println!("Corps de la réponse : {}", res.text()?);
```

L'exemple ci-dessus montre comment envoyer une requête GET avec une authentification de base, mais vous pouvez également utiliser cette méthode pour d'autres types de requêtes ou en utilisant des bibliothèques tierces comme `serde_json` pour envoyer des données JSON.

## Exploration en profondeur

L'authentification de base est une méthode de sécurité simple et largement utilisée dans les applications web. Elle consiste à envoyer le nom d'utilisateur et le mot de passe encodés en base64 dans l'en-tête `Authorization` de la requête HTTP. Cela signifie que ces informations ne sont pas cryptées et peuvent être facilement décodées. C'est pourquoi il est fortement recommandé d'utiliser une connexion sécurisée (HTTPS) lorsque vous utilisez l'authentification de base pour protéger vos données sensibles.

De plus, il est important de prendre en compte les risques de sécurité liés à la gestion des noms d'utilisateur et des mots de passe. Évitez de les stocker en clair dans votre code et utilisez des méthodes telles que le hachage et le salage pour sécuriser ces informations.

## Voir aussi

- [Documentation officielle de la bibliothèque reqwest](https://docs.rs/reqwest/0.11.0/reqwest/index.html)
- [Tutoriel sur les requêtes HTTP en Rust](https://blog.logrocket.com/making-http-requests-in-rust/)
- [Article sur les meilleures pratiques de sécurité en Rust](https://medium.com/csml-research/best-practices-for-secure-rust-code-4bebbb36a1f4)