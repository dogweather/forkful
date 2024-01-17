---
title:                "Envoi d'une requête http avec une authentification de base"
html_title:           "Rust: Envoi d'une requête http avec une authentification de base"
simple_title:         "Envoi d'une requête http avec une authentification de base"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Qu'est-ce que l'envoi d'une requête HTTP avec une authentification de base ?
Envoyer une requête HTTP avec une authentification de base consiste à fournir un nom d'utilisateur et un mot de passe dans les en-têtes de la requête pour accéder à une ressource en ligne protégée. Cela permet aux programmeurs d'obtenir des informations et de soumettre des données à des sites Web sécurisés.

Pourquoi les programmeurs font-ils cela?
L'authentification de base est l'une des méthodes les plus couramment utilisées pour protéger les informations sensibles en ligne. Les programmes qui envoient des requêtes HTTP avec une authentification de base peuvent sécuriser les communications entre eux et les serveurs en s'identifiant mutuellement.

## Comment faire:
Voici un exemple de code en Rust pour envoyer une requête HTTP avec une authentification de base :

```Rust
use reqwest::header::AUTHORIZATION;

let client = reqwest::Client::new();
let mut headers = reqwest::header::HeaderMap::new();

let username = "monnom";
let password = "monmotdepasse";
let auth = format!("Basic {}", base64::encode(format!("{}:{}", username, password)));
headers.insert(AUTHORIZATION, auth.parse().unwrap());

let response = client
    .get("https://exemplo.com/infosensibles")
    .headers(headers)
    .send()
    .await
    .unwrap();

println!("Statut de la réponse : {}", response.status());
println!("Corps de la réponse : {}", response.text().await.unwrap());
```

La sortie de ce code contient le statut de la réponse et le corps de la réponse correspondant à la ressource protégée en ligne.

## Plongée en profondeur:
L'authentification de base a été introduite dans le protocole HTTP en 1999 et reste largement utilisée aujourd'hui. Cependant, elle est considérée comme présentant des risques de sécurité car les informations d'identification sont envoyées en clair dans la requête HTTP. Une alternative plus sécurisée serait d'utiliser une authentification par jeton.

L'implémentation d'une authentification de base dans un programme en Rust peut se faire en utilisant la bibliothèque reqwest et en ajoutant l'en-tête approprié contenant les informations d'identification.

## Voir aussi:
- [Documentations sur l'authentification de base en HTTP](https://www.w3.org/Protocols/HTTP/1.0/spec.html#BasicAA)
- [Bibliothèque reqwest pour envoyer des requêtes HTTP en Rust](https://github.com/seanmonstar/reqwest)