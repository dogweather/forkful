---
title:                "Envoi d'une requête HTTP avec authentification de base"
date:                  2024-01-20T18:01:43.000486-07:00
model:                 gpt-4-1106-preview
simple_title:         "Envoi d'une requête HTTP avec authentification de base"

category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
L'envoi d'une requête HTTP avec authentification de base consiste à fournir un nom d'utilisateur et un mot de passe pour accéder à une ressource protégée sur le web. Les programmeurs le font pour interagir de manière sécurisée avec des API ou des services web nécessitant une vérification d'identité.

## Comment faire :
```Fish Shell
# Encodage des identifiants en base64
set -l credentials (echo -n "username:password" | base64)

# Envoi de la requête avec l'en-tête 'Authorization'
set -l response (curl -H "Authorization: Basic $credentials" https://example.com/resource)

# Affichage de la réponse
echo $response
```

Exemple de sortie :
```
{
  "data": "Contenu protégé auquel vous avez accédé avec succès."
}
```

## Plongée en profondeur
L'authentification de base HTTP a été conçue dans les premiers jours du web et reste un mécanisme simple mais moins sécurisé. Elle est souvent remplacée par des méthodes plus robustes comme OAuth. Cependant, elle est encore utilisée pour sa simplicité quand la sécurité n'est pas une préoccupation critique ou comme première couche de défense.

L'authentification de base consomme peu de ressources car elle n'implique que l'encodage et le décryptage de chaînes de caractères en base64, ce qui est négligeable pour les systèmes modernes. Ce mécanisme n'est pas sécurisé sur des connexions non cryptées car le base64 est facile à décoder. Il est donc crucial de l'utiliser avec HTTPS.

## Voir aussi
- Documentation de Fish Shell : [fishshell.com/docs](https://fishshell.com/docs/current/index.html)
- Guide cURL : [curl.se/docs](https://curl.se/docs/)
- Authentification HTTP sur MDN : [developer.mozilla.org/fr/docs/Web/HTTP/Authentication](https://developer.mozilla.org/fr/docs/Web/HTTP/Authentication)
