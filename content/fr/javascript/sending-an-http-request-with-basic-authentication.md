---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Arduino: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Qu'est-ce que & Pourquoi ?

L'envoi d'une requête HTTP avec authentification de base se réfère à l'utilisation des identifiants (nom d'utilisateur et mot de passe) pour accéder à une ressource via HTTP. Les programmeurs y recourent pour protéger la confidentialité et l'intégrité des échanges entre le client et le serveur.

## Comment faire :

```Javascript
let username = 'user';
let password = 'password';

let headers = new Headers();
headers.set('Authorization', 'Basic ' + btoa(username + ":" + password));

fetch('https://api.monsite.com/data', {headers: headers})
.then(response => response.json())
.then(data => console.log(data))
.catch(error => console.error('Error:', error));
```
Dans cet exemple, nous utilisons l'API Fetch pour envoyer une requête GET à 'https://api.monsite.com/data'. 'btoa' est une fonction JavaScript qui encode les identifiants en base64. En cas de réponse, les données sont affichées dans la console.

## Plongeons profondément :

En termes historiques, l'authentification de base HTTP a été l'une des premières méthodes pour contrôler l'accès aux ressources web. Cependant, elle présente des vulnérabilités évidentes : notamment, le fait qu'elle transmette les informations d'identification en texte clair (bien que codé en base64).

Il existe des alternatives comme l'authentification Digest, l'authentification par jeton et OAuth.Avec l'authentification Digest, les informations d'identification sont transmises sous forme de hachage MD5, qui représente un niveau de sécurité plus élevé. L'authentification par jeton et OAuth sont aujourd'hui largement adoptées dans le développement d'applications web modernes.

Dans l'implémentation mentionnée précédemment, nous avons utilisé la méthode 'btoa' pour encoder les informations d'identification en base64, ce qui n'est pas sécurisé pour les applications de production. En pratique, vous devriez toujours utiliser HTTPS pour protéger vos informations d'identification lors de leur transmission.

## Voir aussi :

Pour en savoir plus sur l'authentification HTTP de base et ses alternatives, consultez :
- [Authentification Basic sur MDN Web Docs](https://developer.mozilla.org/fr/docs/Web/HTTP/Authentication)
- [Authentification Basic sur Wikipedia](https://fr.wikipedia.org/wiki/Authentification_HTTP#Authentification_de_base)
- [Alternatives à l'authentification Basic, sur Stack Overflow](https://stackoverflow.com/questions/549/the-definitive-guide-to-form-based-website-authentication#477579)