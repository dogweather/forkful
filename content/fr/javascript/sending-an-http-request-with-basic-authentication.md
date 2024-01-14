---
title:                "Javascript: Envoi d'une requête http avec authentification de base"
simple_title:         "Envoi d'une requête http avec authentification de base"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes développeur Javascript, il est fort probable que vous ayez entendu parler de l'envoi de requêtes HTTP avec une authentification de base. Mais pourquoi cette action est-elle importante et pourquoi devriez-vous l'utiliser ? La réponse est simple : l'envoi de requêtes HTTP avec une authentification de base est un moyen simple et sécurisé d'accéder à des données qui nécessitent une authentification.

## Comment faire

Pour envoyer une requête HTTP avec une authentification de base en Javascript, vous avez besoin de deux choses : une URL et des informations d'identification (nom d'utilisateur et mot de passe). Voici un exemple de code qui montre comment réaliser cela :

```Javascript
const url = 'https://monsite.com/api/user';
const username = 'mon_username';
const password = 'mon_mot_de_passe';

// création d'un objet pour stocker les informations d'authentification
const headers = new Headers();
headers.set('Authorization', `Basic ${btoa(`${username}:${password}`)}`);

// création d'un objet de configuration pour la requête
const config = {
  method: 'GET',
  headers: headers
};

// envoi de la requête
fetch(url, config)
  .then(response => response.json())
  .then(data => console.log(data))
  .catch(error => console.log(error));
```
Dans cet exemple, nous utilisons la méthode `btoa()` pour encoder les informations d'authentification en base64, qui est le format utilisé pour les requêtes HTTP avec une authentification de base. Ensuite, nous ajoutons ces informations à un objet `Headers` et les incluons dans notre objet de configuration pour la requête. Enfin, nous utilisons la méthode `fetch()` pour envoyer la requête et récupérer les données.

## Deep Dive

Maintenant que nous savons comment envoyer une requête HTTP avec une authentification de base en Javascript, plongeons un peu plus en détail dans le processus. Lorsque vous envoyez une requête avec une authentification de base, les informations d'identification sont incluses dans l'en-tête de la requête sous la forme d'une chaîne encodée en base64. Cela signifie que ces informations ne sont pas sécurisées, car il est facile de déchiffrer une chaîne encodée en base64. Pour une sécurité accrue, il est recommandé d'utiliser une authentification plus avancée, comme l'authentification OAuth2.

Cependant, l'authentification de base reste un moyen simple et couramment utilisé pour accéder aux données protégées par une authentification. Il est important de noter que vous ne devez jamais inclure vos informations d'identification dans votre code source et de vous assurer que votre serveur prend en charge l'authentification de base avant d'envoyer une requête.

## See Also
- [MDN Web Docs - Using Fetch](https://developer.mozilla.org/fr/docs/Web/API/fetch/Utiliser_fetch)
- [MDN Web Docs - Headers API](https://developer.mozilla.org/fr/docs/Web/API/Headers)
- [MDN Web Docs - Basic Authentication](https://developer.mozilla.org/fr/docs/Web/HTTP/Authentication#Basic_authentication_scheme)