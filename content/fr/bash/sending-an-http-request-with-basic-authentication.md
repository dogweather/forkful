---
title:                "Envoyer une demande http avec une authentification de base"
html_title:           "Bash: Envoyer une demande http avec une authentification de base"
simple_title:         "Envoyer une demande http avec une authentification de base"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
L'envoi d'une requête HTTP avec une authentification de base est une méthode utilisée par les programmeurs pour sécuriser leurs requêtes HTTP. Cela permet d'ajouter un niveau de protection en demandant aux utilisateurs de s'authentifier avant d'accéder à une ressource ou une API spécifique.

## Comment faire : 
Voici un exemple d'un envoi de requête HTTP avec une authentification de base en utilisant Bash :

```
curl -u username:password URL
```

Le résultat devrait afficher le contenu de la ressource ou l'état de la requête si elle a réussi ou échoué.

## Plongée en profondeur :
L'authentification de base est une méthode d'authentification HTTP très simple et largement utilisée depuis sa première implémentation en 1996. Bien qu'elle soit facile à mettre en place, elle ne garantit pas une sécurité optimale car les informations d'identification sont envoyées en clair. Des alternatives telles que l'authentification Digest ou l'utilisation de certificats sont souvent préférées pour une meilleure protection des données.

Côté programmation, l'envoi d'une requête avec une authentification de base peut être réalisé à l'aide d'un script Bash ou en utilisant une librairie tierce telle que cURL.

## Voir aussi :
- [Documentation cURL](https://curl.se/docs/auth.html)
- [Article sur l'authentification HTTP](https://developer.mozilla.org/fr/docs/Web/HTTP/Authentication)