---
title:                "Gleam: Envoi d'une requête http"
simple_title:         "Envoi d'une requête http"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Pourquoi

Pourquoi voudriez-vous envoyer une requête HTTP dans votre code Gleam ? Tout simplement pour communiquer avec des serveurs web externes ou récupérer des données à partir d'une API. Les requêtes HTTP sont un moyen efficace de faire interagir votre code avec le monde extérieur et peuvent être utiles pour de nombreuses applications, telles que les applications web ou les chatbots.

## Comment Faire

Voici quelques exemples de code pour vous montrer comment envoyer une requête HTTP en utilisant Gleam :

```Gleam
// Importer le module "http"
import gleam/http

// Définir une fonction pour envoyer une requête GET à l'API de GitHub
pub fn get_github_api() {
  // Définir l'URL de l'API
  let url = "https://api.github.com/users/gleam-lang"

  // Envoyer la requête GET
  let response = http.request({ method: "GET", url: url })

  // Vérifier si la requête a réussi
  case response {
    Ok(body) -> {
      // Le corps de la réponse est stocké dans "body"
      // Vous pouvez maintenant effectuer des opérations sur les données récupérées
      // Par exemple, imprimez le corps de la réponse
      Debug.to_string(body) |> Debug.print
    }
    Err(error) -> {
      // La requête a échoué, traiter l'erreur ici
      Debug.to_string(error) |> Debug.print
    }
  }
}
```

Le code ci-dessus utilise le module "http" de Gleam pour envoyer une requête GET à l'API de GitHub et imprimer le corps de la réponse en cas de succès. Vous pouvez également utiliser des méthodes autres que GET, comme POST ou PUT, en modifiant simplement la valeur de la clé "method" dans la requête.

## Plongée Dans Les Détails

Envoyer une requête HTTP dans Gleam est un processus simple, mais il y a quelques points à garder à l'esprit :

- La fonction "http.request" renvoie un "Record" avec la clé "Ok" ou "Err". Il s'agit d'un type de données de gestion d'erreurs de Gleam qui permet de gérer facilement les exceptions lors de l'envoi de requêtes.
- L'URL peut être stockée dans une variable ou être directement entrée comme valeur de la clé "url" dans la fonction "http.request".
- Vous pouvez également ajouter des en-têtes personnalisés à votre requête en ajoutant une clé "headers" dans le Record de requête et en y stockant un "Map" de clés/valeurs pour chaque en-tête.
- Pour faciliter la lecture et l'utilisation de vos fonctions d'envoi de requête HTTP, vous pouvez créer un module distinct et les importer dans vos autres fichiers Gleam.

## Voir Aussi

- Documentation officielle pour le module "http" de Gleam - https://gleam.run/modules/http.html
- Tutoriel sur la création d'une application API avec Gleam - https://dev.to/gleam_lang/building-a-rest-api-with-gleam-3lhg