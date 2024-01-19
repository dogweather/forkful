---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Arduino: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Envoyer une requête HTTP avec une authentification de base, c'est demander un service à un serveur en fournissant des informations d'identification codées en base 64. Les programmeurs le font pour sécuriser des données sensibles lors de la communication entre le client et le serveur. 

## Comment faire :
Voici un exemple basique de comment envoyer une requête HTTP avec une authentification de base en Rust. 

```Rust
use reqwest::blocking::Client;
use std::collections::HashMap;

fn main() {
    let client = Client::new();
    let mut map = HashMap::new();
    map.insert("lang", "rust");
    
    let res = client.post("https://httpbin.org/post")
        .basic_auth("username", Some("password"))
        .json(&map)
        .send();

    match res {
        Ok(r) => println!("Response: {:?}", r),
        Err(e) => println!("Error: {:?}", e),
    }
}
```
Pour ce code, l'affichage vous donnera quelque chose comme ceci :

```Rust
Response: Response { url: "https://httpbin.org/post", status: 200, headers: {...} }
```

## Plongeon profond
Historiquement, l'authentification de base a été implémentée pour la première fois dans le protocole HTTP/1.0 en 1996. C'est une des méthodes d'authentification les plus simples et les plus anciennes dans le monde de la programmation web.

Pour les alternatives, vous avez l'authentification à jeton comme JWT, OAuth 2.0 pour plus de sécurités. Il existe aussi la méthode d'authentification Digest qui est similaire à l'authentification de base, mais un peu plus sécurisée.

En ce qui concerne les détails de mise en œuvre, requête HTTP avec une authentification de base en Rust, le package `reqwest` est utilisé. En utilisant la méthode `basic_auth`, l'identifiant et le mot de passe sont encodés en base 64 dans l'en-tête HTTP par le package.

## Voir aussi
Voici quelques liens utiles pour une exploration plus approfondie :
- Documentation Rust sur `reqwest`: https://docs.rs/reqwest/
- Standard HTTP/1.1: https://www.rfc-editor.org/rfc/rfc2616.html
- JWT Authentication: https://jwt.io/introduction/