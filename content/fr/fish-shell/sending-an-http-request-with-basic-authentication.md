---
title:                "Envoi d'une requête http avec une authentification de base"
html_title:           "Fish Shell: Envoi d'une requête http avec une authentification de base"
simple_title:         "Envoi d'une requête http avec une authentification de base"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Pourquoi

Si vous êtes un développeur ou un administrateur système, vous avez probablement rencontré des situations où vous devez interagir avec des API ou des services Web qui nécessitent une authentification de base. Dans ces cas, il est essentiel de comprendre comment envoyer une requête HTTP avec une authentification de base dans votre script ou dans votre terminal. Cet article vous expliquera comment le faire en utilisant le Shell Fish.

## Comment faire
Pour envoyer une requête HTTP avec une authentification de base en utilisant Fish Shell, nous allons utiliser la commande `curl` avec le paramètre `-u` pour spécifier le nom d'utilisateur et le mot de passe. Voici un exemple de code qui envoie une requête GET à l'API de GitHub avec un nom d'utilisateur et un mot de passe :

```
curl -u username:password https://api.github.com/user
```

Vous pouvez également utiliser des variables pour stocker le nom d'utilisateur et le mot de passe afin de rendre le code plus lisible et facilement modifiable. Voici un exemple :

```
set username "username"
set password "password"
curl -u $username:$password https://api.github.com/user
```

Lorsque vous exécutez ce code, vous devriez voir une sortie JSON contenant les informations de l'utilisateur connecté.

## Plongée en profondeur
Pour ceux qui veulent en savoir plus sur l'envoi de requêtes HTTP avec une authentification de base en utilisant Fish Shell, voici quelques informations supplémentaires :

- Le paramètre `-u` de la commande `curl` correspond à `--user` et peut également être utilisé avec une valeur sous forme de chaîne au lieu d'utiliser des variables.
- Si vous avez besoin d'envoyer une requête POST avec une authentification de base, vous pouvez utiliser le paramètre `-d` pour spécifier les données à envoyer dans la requête.
- Si l'API ou le service que vous utilisez nécessite une authentification de base avec un système de chiffrement différent de Basic, vous devrez utiliser l'option `--anyauth` de `curl` et spécifier le type d'authentification dans le paramètre `-u`.

Vous pouvez en savoir plus sur ces fonctionnalités en consultant la documentation officielle de Fish Shell et de la commande `curl`.

## Voir aussi
- [Documentation de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Documentation de la commande curl](https://curl.se/docs/manpage.html)