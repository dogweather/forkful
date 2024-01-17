---
title:                "Envoyer une requête http"
html_title:           "Rust: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi ?
L'envoi d'une requête HTTP est le processus par lequel un programme demande des informations à un serveur Web. Les programmeurs le font pour communiquer avec d'autres applications, récupérer des données, ou effectuer des actions sur un site Web.

## Comment faire :
Il existe plusieurs façons d'envoyer une requête HTTP en Rust. Voici un exemple en utilisant la bibliothèque standard :
```Rust
use std::io::{Read, Write};
use std::net::TcpStream;

fn main() {
    let mut stream = TcpStream::connect("www.example.com:80").unwrap();
    stream.write(b"GET / HTTP/1.1\r\nHost: www.example.com\r\n\r\n").unwrap();
    let mut response = String::new();
    stream.read_to_string(&mut response).unwrap();
    println!("{}", response);
}
```
Le code utilise la structure TcpStream pour établir une connexion avec le serveur Web et pour envoyer une requête GET pour la page d'accueil. Le serveur renvoie ensuite une réponse, qui est stockée dans la variable "response" et affichée à l'écran.

## Plongée en profondeur :
L'envoi de requêtes HTTP est un élément essentiel de la communication sur Internet. Il a été introduit dans les années 1990 dans le but de permettre aux différents systèmes informatiques de communiquer entre eux de manière standardisée. En plus de la bibliothèque standard, il existe également des bibliothèques externes telles que Hyper et Req pour faciliter l'envoi de requêtes HTTP en Rust.

## Voir aussi :
Pour en savoir plus sur l'envoi de requêtes HTTP en Rust, vous pouvez consulter les ressources suivantes :
- [Documentation de la bibliothèque standard](https://doc.rust-lang.org/std/net/struct.TcpStream.html)
- [Hyper: une bibliothèque HTTP pour Rust](https://hyper.rs/)
- [Req: une bibliothèque simple pour effectuer des requêtes HTTP en Rust](https://docs.rs/req/0.10.0/req/index.html)