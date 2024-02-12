---
title:                "Envoi d'une requête HTTP avec authentification de base"
aliases:
- /fr/rust/sending-an-http-request-with-basic-authentication/
date:                  2024-01-20T18:02:44.643420-07:00
model:                 gpt-4-1106-preview
simple_title:         "Envoi d'une requête HTTP avec authentification de base"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Envoyer une requête HTTP avec une authentification de base, c'est communiquer avec un serveur Web en fournissant des identifiants (username/password) encodés en Base64. Les programmeurs l'utilisent pour accéder à des ressources sécurisées via des API ou des services Web.

## Comment faire :
```Rust
extern crate reqwest;
extern crate base64;

use reqwest::header::{Authorization, Basic};

fn main() -> Result<(), reqwest::Error> {
    let client = reqwest::blocking::Client::new();
    let user = "utilisateur";
    let password = "motdepasse";
    let encoded_credentials = base64::encode(format!("{}:{}", user, password));
  
    let res = client
        .get("http://exemple.com/route_protégée")
        .header(Authorization(Basic { username: user.to_string(), password: Some(password.to_string()) }))
        .send()?;

    println!("Status: {}", res.status());
    println!("Headers:\n{:?}", res.headers());
  
    Ok(())
}
```
Sortie attendue :
```
Status: 200 OK
Headers:
{
    // Détails d'en-tête typiques ici
}
```

## Plongée Profonde
Historiquement, l'authentification de base a été l'un des premiers mécanismes pour sécuriser les communications HTTP, mais elle n'est pas aussi sûre que des méthodes modernes comme OAuth. En Rust, la bibliothèque `reqwest` est souvent utilisée pour les requêtes HTTP, tandis qu'`Authorization` et `Basic` proviennent de ses modules `header`. On encode les créances en Base64 pour suivre le standard HTTP, mais notez que cela n'offre pas de chiffrement. Les alternatives incluent des systèmes d'authentification plus complexes comme les jetons Bearer, l'authentification Digest, ou même utiliser HTTPS pour ajouter une couche de sécurité avec SSL/TLS.

## Voir aussi
- La documentation `reqwest` pour une exploration plus poussée : [reqwest doc](https://docs.rs/reqwest/)
- Le RFC 7617 du standard HTTP Basic authentication : [RFC 7617](https://tools.ietf.org/html/rfc7617)
- Une introduction à l'encodage Base64 : [Base64 Encoding](https://en.wikipedia.org/wiki/Base64)
