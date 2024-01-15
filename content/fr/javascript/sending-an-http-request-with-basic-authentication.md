---
title:                "Envoyer une demande http avec une authentification de base"
html_title:           "Javascript: Envoyer une demande http avec une authentification de base"
simple_title:         "Envoyer une demande http avec une authentification de base"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi envoyer une requête HTTP avec une authentification de base?

L'authentification de base est un moyen simple et courant pour sécuriser les connexions entre un client et un serveur. En utilisant une combinaison d'identifiant et de mot de passe, le serveur peut vérifier l'identité du client avant de lui donner accès aux ressources protégées. Cela est particulièrement utile pour les applications nécessitant une authentification, telles que les API et les sites web sécurisés.

## Comment le faire?

```Javascript
const username = "mon_nom_d_utilisateur";
const password = "mon_mot_de_passe";
const url = "https://exemple.com/api/ressource";

fetch(url, {
  headers: {
    Authorization: "Basic " + btoa(username + ":" + password)
  }
})
.then(response => response.json())
.then(data => console.log(data));
```

Dans cet exemple, nous utilisons la méthode `fetch` pour envoyer une requête HTTP vers l'URL spécifiée. La partie clé est la ligne `Authorization` dans le header de la requête qui contient le mot-clé "Basic" suivi de l'encodage Base64 de la combinaison "nom d'utilisateur: mot de passe".

Lorsque le serveur reçoit la requête, il décode l'encodage Base64 pour obtenir la combinaison d'identifiant et de mot de passe. Il peut alors vérifier ces informations et donner ou refuser l'accès en conséquence.

## Plongée en profondeur

L'authentification de base est une méthode simple mais pas très sécurisée car les informations de connexion sont facilement visibles dans le header de la requête. Pour une meilleure sécurité, il est recommandé d'utiliser l'authentification par jeton (token-based authentication) qui utilise un jeton d'accès au lieu d'un nom d'utilisateur et d'un mot de passe.

Il est également important de noter que l'authentification de base ne chiffre pas les informations d'identification, donc si vous utilisez une connexion non sécurisée (http), ces informations peuvent être interceptées par des tiers. Il est donc préférable de toujours utiliser une connexion sécurisée (https) lors de l'utilisation de l'authentification de base.

## Voir aussi

- [MDN Web Docs: HTTP Authentication](https://developer.mozilla.org/fr/docs/Web/HTTP/Authentication)
- [Blog d'Okta: Basic vs Token-Based Authentication](https://developer.okta.com/blog/2019/06/04/basic-vs-token-based-authentication)
- [Blog de Cloudflare: HTTP Basic Auth: How Secure is It?](https://blog.cloudflare.com/http-basic-auth-how-secure-is-it/)