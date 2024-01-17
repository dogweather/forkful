---
title:                "Envoi d'une requête http avec authentification de base"
html_title:           "Fish Shell: Envoi d'une requête http avec authentification de base"
simple_title:         "Envoi d'une requête http avec authentification de base"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est & Pourquoi?

Envoyer une requête HTTP avec une authentification basique est une façon courante pour les programmeurs d'accéder à des ressources protégées sur le web. Cela permet d'ajouter une couche de sécurité en utilisant un identifiant et un mot de passe pour accéder à des données sensibles.

# Comment faire:

Voici comment envoyer une requête HTTP avec une authentification basique en utilisant le 'Fish Shell':

```Fish Shell

# Premièrement, importez la bibliothèque Curl:

source /dev/stdin <<< "$(curl -fsSL -HCache-Control:no-cache \
                       https://raw.githubusercontent.com/fish-shell/fish-shell/master/share/tools/web_config.fish)"

# Ensuite, utilisez la commande 'curl' avec l'option '-u' pour spécifier l'identifiant et le mot de passe:

curl -u username:password https://example.com

```

# Approfondissement:

- Contexte historique: L'authentification basique a été introduite dans HTTP en 1996 dans la RFC 2068.
- Alternatives: Il existe d'autres méthodes d'authentification plus sécurisées telles que l'authentification par token ou OAuth.
- Détails d'implémentation: L'authentification basique utilise les entêtes 'Authorization' et 'WWW-Authenticate' pour envoyer et recevoir les informations d'identification.

# Voir aussi:

Pour plus d'information sur l'authentification basique avec Fish Shell, vous pouvez consulter ces sources:

- La documentation officielle sur l'utilisation de Curl avec Fish Shell: https://fishshell.com/docs/current/cmds/curl.html
- Le tutoriel sur comment envoyer une requête HTTP avec authentification basique en utilisant Curl et Fish Shell: https://medium.com/@younup/using-curl-with-basic-authentication-in-fish-shell-696a01b3e29e