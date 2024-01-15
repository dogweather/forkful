---
title:                "Envoi d'une demande http avec une authentification de base"
html_title:           "Rust: Envoi d'une demande http avec une authentification de base"
simple_title:         "Envoi d'une demande http avec une authentification de base"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous pourriez vous demander pourquoi vous devriez vous intéresser à envoyer une requête HTTP avec une authentification de base. Eh bien, cela peut être très utile lorsque vous devez accéder à des informations sensibles sur un serveur, comme des données utilisateurs ou des configurations de sécurité.

## Comment faire

Ecrire du code Rust pour envoyer une requête HTTP avec une authentification de base est assez simple. Tout d'abord, vous devez importer les packages nécessaires :

```Rust
use reqwest::Client; // pour envoyer des requêtes HTTP
use base64; // pour encoder les informations d'authentification
```

Ensuite, vous devrez créer une instance du client HTTP et lui passer l'URL de la ressource que vous souhaitez accéder ainsi que les informations d'authentification encodées en base64 :

```Rust
let client = Client::new(); // crée une instance du client HTTP
let url = "https://example.com/api/user/1"; // l'URL de la ressource à accéder
let auth_info = "username:password"; // les informations d'authentification
let base64_auth = base64::encode(auth_info); // encode les informations en base64
let response = client.get(url)
    .header(reqwest::header::AUTHORIZATION, format!("Basic {}", base64_auth)) // ajoute l'en-tête d'authentification
    .send() // envoie la requête
    .await?; // attend la réponse du serveur
```

Enfin, vous pouvez traiter la réponse et récupérer les données dont vous avez besoin :

```Rust
// Vérifie si la requête a réussi
if response.status() == 200 {
    // Récupère le corps de la réponse en tant que texte
    let body = response.text().await?;
    // Fait des opérations avec les données reçues
    println!("Données utilisateur : {}", body);
} else {
    // Affiche une erreur si la requête a échoué
    println!("Erreur : {}", response.status());
}
```

Et c'est tout ! Vous avez maintenant envoyé une requête HTTP avec une authentification de base en utilisant Rust.

## Plongée en profondeur

Les informations d'authentification encodées en base64 sont une manière basique de sécuriser les données transmises lors d'une requête HTTP. Cependant, elle peut être facilement décodée par quelqu'un qui intercepte la requête. Il est donc recommandé d'utiliser des méthodes d'authentification plus avancées, telles que OAuth, pour une meilleure sécurité.

## Voir aussi

- Documentation officielle de Rust : https://www.rust-lang.org/fr/
- Crates.io (répertoire de crates Rust) : https://crates.io/
- Tutoriel sur l'envoi de requêtes HTTP avec Rust : https://dev.to/akatch/how-to-make-http-requests-in-rust-1229