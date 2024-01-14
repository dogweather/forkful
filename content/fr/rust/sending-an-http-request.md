---
title:                "Rust: Envoyer une demande http"
simple_title:         "Envoyer une demande http"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Pourquoi
Les requêtes HTTP sont une partie essentielle de nombreuses applications modernes, permettant aux utilisateurs de communiquer avec des serveurs pour obtenir du contenu et des données. Apprendre comment envoyer une requête HTTP en utilisant Rust peut vous permettre de créer des applications plus rapides, plus fiables et plus sécurisées.

## Comment Faire
Voici un exemple de code Rust pour envoyer une requête HTTP simple:

```Rust
use std::io::{Read, Result};
use std::net::TcpStream;

fn main() -> Result<()> {
    let mut stream = TcpStream::connect("httpbin.org:80")?;
    stream
        .write(b"GET /get HTTP/1.1\r\nHost: httpbin.org\r\nConnection: close\r\n\r\n")?;
    let mut buffer = String::new();
    stream.read_to_string(&mut buffer)?;
    println!("{}", buffer);
    Ok(())
}
```

Cet exemple utilise la bibliothèque standard de Rust pour établir une connexion TCP avec le serveur et envoyer une requête GET pour obtenir un exemple de réponse. Ensuite, le contenu de la réponse est lu et affiché sur la console. En utilisant les méthodes appropriées pour écrire et lire les données, nous pouvons facilement envoyer une requête HTTP et obtenir la réponse à l'aide de Rust.

## Plongée en Profondeur
Une fois que vous avez compris les bases de l'envoi de requêtes HTTP en utilisant Rust, vous pouvez explorer d'autres bibliothèques populaires telles que `reqwest` ou `hyper` qui offrent une API plus conviviale et des fonctionnalités plus avancées telles que la gestion des cookies, le streaming de données et la prise en charge de protocoles supplémentaires tels que HTTPS.

De plus, vous pouvez apprendre à manipuler les différentes parties d'une requête HTTP, telles que les en-têtes et le corps, en utilisant les structures de données telles que `HashMap` et `Vec` disponibles dans la bibliothèque standard.

# Voir Aussi
- [La documentation officielle sur les requêtes HTTP en Rust](https://doc.rust-lang.org/std/io/trait.Write.html)
- [Exemple de code pour l'envoi de requêtes HTTP avec `reqwest`](https://github.com/seanmonstar/reqwest)
- [Exemple de code pour l'envoi de requêtes HTTP avec `hyper`](https://github.com/hyperium/hyper)