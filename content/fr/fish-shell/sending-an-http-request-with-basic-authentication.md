---
title:                "Fish Shell: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur ou un administrateur système, il y a de fortes chances que vous ayez déjà eu besoin d'envoyer une requête HTTP avec une authentification de base. Peut-être que vous travaillez avec des APIs ou que vous avez besoin d'accéder à un site protégé par un nom d'utilisateur et un mot de passe. Dans cet article, nous allons explorer comment le faire en utilisant Fish Shell.

## Comment faire

Tout d'abord, nous allons installer le package HTTPie, qui est un outil de ligne de commande pour envoyer des requêtes HTTP.

```
Fish Shell> apt install httpie
```

Ensuite, nous allons créer une requête HTTP avec une authentification de base en utilisant l'option `-a` suivie du nom d'utilisateur et du mot de passe :

```
Fish Shell> http -a username:password https://example.com/api
```

Vous allez recevoir une réponse contenant les informations demandées à partir de l'API ou du site protégé.

## Plongée en profondeur

Maintenant, voyons comment fonctionne réellement l'authentification de base dans une requête HTTP. Lorsque vous envoyez une requête avec une authentification de base, le nom d'utilisateur et le mot de passe sont encodés en utilisant Base64 et inclus dans l'en-tête de la requête comme ceci :

```
Authorization: Basic dXNlcm5hbWU6cGFzc3dvcmQ=
```

Le serveur vérifie ensuite les informations d'identification en décodant l'en-tête et en les comparant à celles stockées dans son système. Si elles correspondent, la requête est autorisée et vous recevez une réponse.

## Voir aussi

- [Documentation HTTPie](https://httpie.org/doc)
- [Explication de Base64](https://fr.wikipedia.org/wiki/Base64)
- [Tutoriel sur les requêtes HTTP avec Fish Shell](https://www.gouminapp.com/articles/tutorial-how-to-make-http-requests-with-fish-shell)