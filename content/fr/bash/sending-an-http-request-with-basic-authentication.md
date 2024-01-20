---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Bash: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
La requête HTTP avec authentification de base est un moyen pour les programmes d'envoyer des informations à travers le Web avec une sécurité élémentaire. Les programmeurs l'utilisent pour communiquer avec des serveurs qui nécessitent une authentification simple, comme celui d'une API REST.

## Comment faire :
Pour envoyer une requête HTTP avec authentification de base en Bash, on utilisera souvent l'outil curl. Le format est le suivant :

```Bash
curl -u utilisateur:motdepasse http://url-du-serveur
```

L'utilisateur et le mot de passe sont inclus dans la commande. Voici un exemple avec de faux identifiants pour une API imaginaire :

```Bash
curl -u azerty:123456 http://mon-api.imaginaire.com
```

Vous devriez voir une réponse du serveur dans la console. Si vous voyez une erreur 401, c'est que vos identifiants sont incorrects.

## Plongée en profondeur
Historiquement, l'authentification de base HTTP est l'une des formes d'authentification les plus anciennes sur le Web. Cependant, elle est considérée comme peu sécurisée car elle ne chiffre pas les identifiants en eux-mêmes. C'est pourquoi elle est souvent utilisée en combinaison avec un protocole sécurisé comme HTTPS.

Il existe des alternatives, comme l'authentification par jeton ou l'authentification OAuth, qui sont plus sécurisées mais aussi plus complexes à mettre en place.

En terme d'implémentation, notons que curl cache vos identifiants le temps de la session, ce qui peut être à la fois pratique et potentiellement risqué. C'est une chose à garder en tête lorsque vous utilisez ce type d'authentification.

## À voir également
[Aperçu des méthodes d'authentification HTTP](https://developer.mozilla.org/fr/docs/Web/HTTP/Authentication)
[Manuel du curl en français](https://curl.se/docs/manpage.html)
[Cours en ligne sur la sécurité des APIs](https://openclassrooms.com/fr/courses/6573231-design-your-restful-api-with-uml)