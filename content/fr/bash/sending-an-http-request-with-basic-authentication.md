---
title:                "Bash: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi

L'envoi d'une demande HTTP avec une authentification de base est une pratique courante pour sécuriser les informations échangées entre un client et un serveur. Cela est particulièrement important lors de l'utilisation d'applications ou d'APIs qui nécessitent un accès restreint.

## Comment faire

Pour envoyer une demande HTTP avec une authentification de base en Bash, vous pouvez utiliser la commande `curl` suivie du nom de l'URL et des options `-u` pour spécifier le nom d'utilisateur et le mot de passe. Par exemple :

```Bash
curl -u username:password https://api.example.com
```

Si l'authentification réussit, vous recevrez une réponse avec un code d'état 200 et les données demandées. Sinon, vous recevrez une erreur d'authentification.

## Plongée en profondeur

L'authentification de base utilise une méthode de chiffrement simple pour sécuriser les informations d'identification. Le nom d'utilisateur et le mot de passe sont encodés en utilisant la norme de base64, ce qui signifie qu'ils sont facilement décodables par des tiers. Il est donc important de coupler l'authentification de base avec des protocoles de sécurité plus avancés pour une meilleure protection des données.

Lors de l'utilisation de l'authentification de base, il est recommandé de ne pas stocker le nom d'utilisateur et le mot de passe en clair dans le code. Vous pouvez plutôt les stocker dans des variables d'environnement ou dans un fichier de configuration séparé.

## Voir aussi

- [Documentation officielle de curl](https://curl.se/docs/manpage.html)
- [Tutoriel sur l'utilisation de curl pour envoyer des demandes avec une authentification de base](https://www.programiz.com/curl)
- [Article sur la sécurité des demandes HTTP et l'importance de l'authentification de base](https://www.cloudflare.com/learning/security/glossary/basic-authentication/)