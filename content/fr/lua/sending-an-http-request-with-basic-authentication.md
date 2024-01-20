---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Arduino: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Qu'est-ce que & Pourquoi?
L'envoi d'une demande HTTP avec une authentification de base est une méthode courante d'authentification sur le web. Les programmeurs l'utilisent pour protéger les ressources en ligne contre les accès non autorisés.

## Comment faire:
Voici un exemple en Lua pour envoyer une demande HTTP avec une authentification de base :

```Lua
-- Nécessite le module 'socket.http'
local http = require('socket.http')
-- Nécessite le module 'ltn12'
local ltn12 = require('ltn12')

-- URL de la ressource
local url = 'http://example.com'

-- Authentification de base
local auth = 'Basic ' .. (mime.b64('user:password'))

-- Prépare la demande
local response = {}
local r, c, h = http.request{
  url = url,
  sink = ltn12.sink.table(response),
  headers = {
    authorization = auth
  }
}

-- Affiche la réponse
print(table.concat(response, ''))
```

Exemple de sortie :
```Lua
<!DOCTYPE html>
<html>
<body>
Bienvenue sur example.com!
</body>
</html>
```

## Approfondissement
Historiquement, l'envoi de requêtes HTTP avec authentification de base était l'une des premières méthodes d'authentification utilisées sur le web. Cependant, elle transfère les informations d'identification en clair (bien qu'encodées en base64), ce qui n'est pas sûr si la connexion n'est pas sécurisée par SSL/TLS.

Les alternatives à l'authentification de base incluent l'authentification Digest (plus sûre, mais encore rarement utilisée), l'authentification par formulaire (où les informations d'identification sont envoyées dans le corps d'une requête POST), et l'authentification par jeton (très populaire pour les API REST).

Dans l'exemple de code ci-dessus, nous avons utilisé les modules 'socket.http' et 'ltn12' de la bibliothèque LuaSocket pour envoyer la requête HTTP, et le module 'mime' pour encoder les informations d'identification en base64.

## Voir aussi
- [Documentation LuaSocket](http://w3.impa.br/~diego/software/luasocket/http.html)
- [RFC 2617 - HTTP Authentication](https://www.ietf.org/rfc/rfc2617.txt)
- [Présentation de l'authentification HTTP](https://developer.mozilla.org/fr/docs/Web/HTTP/Authentication)