---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Javascript: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

L'envoi d'une requête HTTP avec une authentification de base est une méthode utilisée par les programmeurs pour sécuriser les communications entre un client et un serveur. Cela permet de s'assurer que seules les personnes autorisées ont accès aux informations échangées.

## Comment faire:

```Javascript
fetch('https://mon-serveur.com/api/posts', { // l'url de l'API à appeler
  headers: {
    Authorization: 'Basic ' + btoa('utilisateur:mdp') // encodage en base64 du nom d'utilisateur et du mot de passe
  }
})
.then((response) => {
  return response.json(); // convertit la réponse en JSON
})
.then((data) => {
  console.log(data); // affiche les données récupérées
});
```

L'exemple ci-dessus utilise la méthode [fetch()](https://developer.mozilla.org/fr/docs/Web/API/Fetch_API) pour envoyer une requête HTTP vers une API sécurisée par une authentification de base. Puisque les informations de connexion sont encodées en base64, il est important de souligner que cela ne constitue pas une méthode de sécurisation fiable et qu'il est recommandé d'utiliser une méthode plus robuste telle que l'authentification par jetons (token authentication).

## Plongée en profondeur:

L'authentification de base a été introduite dans la version 1.0 de HTTP en 1996 et continue d'être utilisée aujourd'hui. Cependant, de nombreuses méthodes d'authentification alternatives ont émergé, offrant une meilleure sécurité et plus de flexibilité. Parmi elles, on peut citer l'OAuth, l'OpenID Connect, et l'authentification par jetons. L'authentification de base fonctionne en encodant en base64 les informations de connexion, ce qui peut être facilement décodé par une application tierce.

## Voir aussi:

- [MDN Web Docs - fetch()](https://developer.mozilla.org/fr/docs/Web/API/Fetch_API)
- [Introduction à l'authentification HTTP](https://developer.mozilla.org/fr/docs/Web/HTTP/Authentication)