---
title:                "TypeScript: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi

L'envoi de requêtes HTTP avec une authentification de base est une étape cruciale pour les développeurs TypeScript qui souhaitent établir une communication sécurisée entre leur application et un serveur web. Cette méthode d'authentification permet de s'assurer que seules les personnes autorisées peuvent accéder aux ressources protégées par le serveur.

## Comment faire

Pour envoyer une requête HTTP avec une authentification de base en utilisant TypeScript, nous allons utiliser l'API Fetch, qui est un moyen moderne et simple pour effectuer des appels réseau. Voici un exemple de code qui envoie une requête GET avec une authentification de base :

```TypeScript
// Définition des informations de connexion
const username = "mon_nom_d'utilisateur";
const password = "mon_mot_de_passe";

// Définition de l'URL de la ressource à accéder
const url = "https://exemple.com/api/ressource";

// Construction de l'en-tête d'authentification de base en encodant les informations de connexion en base64
const authHeader = "Basic " + btoa(username + ":" + password);

// Envoi de la requête GET avec l'en-tête d'authentification
fetch(url, {
  headers: {
    "Authorization": authHeader
  }
})
  // Récupération de la réponse au format JSON
  .then(response => response.json())
  .then(data => console.log(data))
  // Gestion des erreurs
  .catch(error => console.error(error));
```

Lorsque nous exécutons ce code, nous devrions voir la réponse du serveur affichée dans la console, à condition que les informations de connexion soient correctes.

## Plongée en profondeur

Pour comprendre comment fonctionne l'authentification de base dans les requêtes HTTP, il est important de connaître la structure de l'en-tête d'authentification. Comme nous l'avons vu dans l'exemple précédent, l'en-tête d'authentification de base est construit en concaténant le nom d'utilisateur et le mot de passe, séparés par un colon, puis en encodant le tout en base64.

Lorsque le serveur reçoit cette en-tête, il décode les informations en base64 et vérifie si les informations de connexion sont valides. Si c'est le cas, le serveur renvoie une réponse avec un code d'état HTTP de 200, indiquant que la requête est traitée avec succès.

Il est important de noter que l'authentification de base n'est pas considérée comme une méthode de sécurité robuste car les informations de connexion sont facilement décodables en base64. Il est donc recommandé d'utiliser d'autres méthodes d'authentification plus sécurisées pour les applications qui traitent des données sensibles.

## Voir aussi

- [Guide sur l'utilisation de Fetch API avec TypeScript](https://www.digitalocean.com/community/tutorials/typescript-fetch-api)
- [Documentation officielle de TypeScript](https://www.typescriptlang.org/docs/)
- [Guide sur les différentes méthodes d'authentification en HTTP](https://developer.mozilla.org/fr/docs/Web/HTTP/Authentication)