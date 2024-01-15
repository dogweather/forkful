---
title:                "Envoi d'une requête http avec authentification de base"
html_title:           "Bash: Envoi d'une requête http avec authentification de base"
simple_title:         "Envoi d'une requête http avec authentification de base"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez dans le domaine de la technologie, il est probable que vous ayez besoin d'envoyer des requêtes HTTP avec une authentification de base pour accéder à des ressources protégées par un mot de passe ou un nom d'utilisateur. Savoir comment le faire peut vous aider à automatiser des tâches, à créer des scripts ou à accéder à des API.

## Comment faire

```Bash
# Utilisez curl pour envoyer une requête GET avec une authentification de base
curl -u username:password https://example.com

# Utilisez wget pour télécharger un fichier protégé par une authentification de base
wget --user=username --password=password https://example.com/file.zip
``` 

## Plongée en profondeur

Lorsque vous envoyez une requête HTTP avec une authentification de base, vous devez inclure un en-tête `Authorization` qui contient le mot-clé "Basic" suivi de l'encodage en base64 du nom d'utilisateur et du mot de passe. Cet encodage est utilisé pour sécuriser les informations sensibles au lieu de les envoyer en clair sur le réseau.

## Voir aussi

- [Basic access authentication sur Wikipedia](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [Tutorial de curl](https://curl.se/docs/httpscripting.html)
- [Documentation de wget](https://www.gnu.org/software/wget/)