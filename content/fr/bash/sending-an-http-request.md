---
title:                "Envoi d'une requête http"
html_title:           "Bash: Envoi d'une requête http"
simple_title:         "Envoi d'une requête http"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Envoyer une requête HTTP est un moyen pour les programmeurs d'interagir avec d'autres sites ou serveurs en utilisant le protocole HTTP. Cela leur permet d'accéder à des ressources, telles que des informations ou des données, disponibles sur ces sites ou serveurs.

## Comment faire:

Voici un exemple de code Bash pour envoyer une requête HTTP à un site web spécifique et en afficher la réponse:

```Bash
curl -i https://www.example.com
```

Ce code utilise la commande ```curl``` pour envoyer la requête et l'option ```-i``` pour afficher la réponse complète, y compris les en-têtes de la requête et de la réponse.

Lorsque vous exécutez le code, vous devriez voir une réponse comme celle-ci:

```
HTTP/1.1 200 OK
Server: nginx
Date: Tue, 25 May 2021 19:12:50 GMT
Content-Type: text/html; charset=UTF-8
Content-Length: 29148
Connection: keep-alive
```

La réponse contient des informations sur le serveur, la date, le type de contenu et la longueur du contenu. En dessous de ces en-têtes, vous trouverez le contenu du site web lui-même.

## Plongée en profondeur:

L'envoi de requêtes HTTP est une partie importante de la programmation en raison de la grande quantité de données disponibles sur le web. Avant les protocoles HTTP, les programmeurs devaient utiliser des protocoles plus anciens et plus limités pour communiquer avec des sites extérieurs.

Bien que la commande ```curl``` soit le moyen le plus courant d'envoyer une requête HTTP en Bash, il existe également d'autres options telles que ```wget``` et les bibliothèques spécialisées en Bash pour l'envoi de requêtes.

## Voir aussi:

Pour en savoir plus sur l'envoi de requêtes HTTP en Bash, consultez ces sources:

- [La documentation officielle de curl](https://curl.se/docs/manpage.html)
- [Un tutoriel sur l'utilisation de curl en Bash](https://www.baeldung.com/curl-rest)