---
title:                "Envoyer une requête http"
html_title:           "Fish Shell: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

Envoyer une requête HTTP est une action essentielle pour les programmeurs. Cela leur permet d'établir une communication avec un serveur web pour récupérer des données ou effectuer des actions. Les requêtes HTTP sont utilisées pour créer des applications web dynamiques et interagir avec des API.

## Comment faire:

```
Fish Shell est un outil puissant pour envoyer des requêtes HTTP. Voici comment vous pouvez l'utiliser:

# Pour envoyer une requête GET simple:
curl http://www.example.com

# Pour inclure des en-têtes avec la requête:
curl -H "Content-Type: application/json" http://www.example.com

# Pour envoyer une requête POST avec des données:
curl -X POST -d '{"username":"John", "password":"12345"}' http://www.example.com/login

# Pour afficher le code de statut de la réponse:
curl -s -o /dev/null -w "%{http_code}" http://www.example.com

```

## Plongée en profondeur:

Les requêtes HTTP ont été inventées en 1989 par Tim Berners-Lee, le fondateur du World Wide Web. Le protocole a évolué au fil du temps et est maintenant utilisé largement pour les échanges de données sur le web. Bien que curl soit l'outil standard pour envoyer des requêtes HTTP en ligne de commande, il existe également d'autres alternatives telles que wget ou httpie. Les programmeurs peuvent également implémenter leurs propres fonctions pour envoyer des requêtes HTTP en utilisant des bibliothèques telles que Requests ou cURL.

## À voir également:

- [Documentation Fish Shell](https://fishshell.com/docs/current/cmds/curl.html)
- [Tutorial Curl](https://curl.haxx.se/docs/httpscripting.html)
- [Autres utilitaires pour envoyer des requêtes HTTP](https://www.quora.com/How-can-I-make-HTTP-requests-without-using-curl)