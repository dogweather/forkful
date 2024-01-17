---
title:                "Envoi d'une requête http avec une authentification de base"
html_title:           "TypeScript: Envoi d'une requête http avec une authentification de base"
simple_title:         "Envoi d'une requête http avec une authentification de base"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
L'envoi d'une requête HTTP avec une authentification de base est un moyen pour les programmeurs d'envoyer des informations sensibles de manière sécurisée. Cela permet de s'assurer que seuls les utilisateurs autorisés peuvent accéder aux données.

## Comment faire:
Voici un exemple de code TypeScript montrant comment envoyer une requête HTTP avec une authentification de base:

```
import axios from 'axios';

const username = 'username';
const password = 'password';

axios.get('https://exemple.com', {
  auth: {
    username,
    password
  }
})
.then(response => {
  console.log(response.data);
})
.catch(error => {
  console.log(error.message);
});
```

L'exemple ci-dessus utilise la bibliothèque axios pour effectuer la requête HTTP et passe les informations d'authentification sous forme de paramètres dans l'objet de configuration de la requête.

## Plongée en profondeur:
L'authentification de base dans les requêtes HTTP est un concept qui a été introduit dans le standard de base HTTP en 1996. Elle est considérée comme plutôt simple et peu sécurisée en comparaison à d'autres méthodes d'authentification. Une alternative populaire et plus sécurisée est l'authentification par token.

Cependant, l'authentification de base reste utilisée dans certains cas où un niveau de sécurité moins élevé est acceptable.

Dans l'exemple de code ci-dessus, les informations d'authentification sont transmises en clair dans la requête, ce qui peut être un risque pour les données sensibles. Il est donc important d'utiliser des certificats SSL pour chiffrer les données lors de leur transmission.

## Voir aussi:
- [MDN Web Docs - Basic Authentication](https://developer.mozilla.org/fr/docs/Web/HTTP/Authentication#Authentification_de_base)
- [Axios Documentation](https://axios-http.com/docs/intro)
- [Utiliser SSL pour sécuriser les connexions HTTP](https://developer.mozilla.org/fr/docs/Glossaire/SSL)