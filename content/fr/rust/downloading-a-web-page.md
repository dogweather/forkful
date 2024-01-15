---
title:                "Téléchargement d'une page web"
html_title:           "Rust: Téléchargement d'une page web"
simple_title:         "Téléchargement d'une page web"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Pourquoi

Tu veux savoir comment télécharger une page web en Rust ? Eh bien, tu es au bon endroit ! Que tu sois un développeur en herbe ou un expert en Rust, cet article te donnera toutes les informations dont tu as besoin pour réussir.

## Comment faire

Télécharger une page web en Rust est assez simple. Tout d'abord, tu auras besoin d'importer la bibliothèque `reqwest` dans ton projet. Ensuite, tu peux utiliser la fonction `get` pour obtenir le contenu de la page web désirée. Voici un exemple de code :

```Rust
extern crate reqwest;

use std::io::Read;

fn main() {
    let mut response = reqwest::get("https://example.com").unwrap();

    let mut content = String::new();
    response.read_to_string(&mut content).unwrap();

    println!("{}", content);
}
```

Et voici le résultat de l'exécution du code :

```text
<!doctype html>
<html>
<head>
    <title>Example Domain</title>

    <meta charset="utf-8" />
    <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
...
```

Comme tu peux le voir, le contenu de la page est stocké dans une chaîne de caractères. Tu peux ensuite le manipuler comme tu le souhaites.

## Plongée en profondeur

Maintenant que tu sais comment télécharger une page web en Rust, tu voudras peut-être découvrir quelques fonctionnalités supplémentaires. La bibliothèque `reqwest` offre de nombreuses options pour personnaliser ta requête, telles que l'ajout d'en-têtes et de paramètres, ainsi que la gestion des erreurs. Tu peux aussi utiliser des méthodes comme `post` ou `put` pour envoyer des données avec ta requête. N'hésite pas à consulter la documentation officielle pour en savoir plus.

## Voir aussi

- [Documentation officielle de la bibliothèque `reqwest`](https://docs.rs/reqwest)
- [Tutoriel pour débuter avec Rust](https://doc.rust-lang.org/book/ch01-00-getting-started.html)
- [Exemple de projet utilisant `reqwest`](https://github.com/seanmonstar/reqwest/tree/master/examples)