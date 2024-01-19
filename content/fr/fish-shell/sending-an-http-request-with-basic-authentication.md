---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Arduino: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Envoyer une requête HTTP avec authentification de base en Fish Shell

## Quoi et Pourquoi ?
L'envoi d'une requête HTTP avec authentification de base implique l'envoi de données sécurisées via une requête HTTP standard. Les programmeurs font cela pour protéger l'accès aux données sensibles et réservées aux utilisateurs validés.

## Comment faire :
Dans Fish Shell, vous pouvez le faire de manière assez simple. Jetez un œil au code ci-dessous:

```fish
function http_auth_basic
    set -l auth (printf "%s" "$argv[1]":"$argv[2]" | base64)
    curl -H "Authorization: Basic $auth" $argv[3]
end
```

Et voici comment ça marche :

```fish
http_auth_basic user password https://exempled'url.com
```

## Plongée Plus Profonde
Historiquement, l'authentification de base a été l'un des moyens les plus simples de sécuriser les échanges de données HTTP. Cependant, en raison de sa simplicité même, elle présente certaines failles de sécurité. Dans le Shell Fish, nous utilisons l'encodage base64 pour augmenter le niveau de sécurité.

Il existe également des alternatives à cette méthode. Par exemple, token bearer et digest access sont deux méthodes populaires qui sont considérées comme plus sûres.

En ce qui concerne l'implémentation, c'est assez direct. Nous utilisons d'abord la commande `printf` pour formater nos informations d'identification en une seule chaîne. Ensuite, nous les encodons en base64. Enfin, nous utilisons `curl` avec l'option `-H` pour envoyer la requête avec l'authentification de base.

## Voir Aussi
Pour en savoir plus sur l'authentification de base et comment l'utiliser dans d'autres shells, certains des liens ci-dessous pourraient être utiles:

- [HTTP Basic Authentication](https://developer.mozilla.org/fr/docs/Web/HTTP/Authentication#basic_authentication)
- [Curl with HTTP Authentication](https://curl.haxx.se/docs/httpscripting.html#HTTP_Authentication)
- [Fish Shell Official Documentation](https://fishshell.com/docs/current/index.html)