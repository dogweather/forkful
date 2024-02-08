---
title:                "Envoi d'une requête HTTP avec authentification de base"
aliases:
- fr/lua/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:01:56.679912-07:00
model:                 gpt-4-1106-preview
simple_title:         "Envoi d'une requête HTTP avec authentification de base"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Envoyer une requête HTTP avec une authentification de base, c'est transmettre des données à un serveur tout en fournissant un nom d'utilisateur et un mot de passe codés en base64. Les programmeurs utilisent cette méthode pour accéder à des ressources sécurisées sur le web.

## Comment faire :

```Lua
local http = require("socket.http")
local ltn12 = require("ltn12")
local mime = require("mime")

-- Encoder vos identifiants
local username = "votre_nom_d'utilisateur"
local password = "votre_mot_de_passe"
local auth = "Basic " .. mime.b64(username .. ":" .. password)

-- Préparer votre requête
local response_body = {}
local res, code = http.request {
  url = "http://exemple.com/chemin/vers/la/ressource",
  method = "GET",
  headers = {
    ["Authorization"] = auth
  },
  sink = ltn12.sink.table(response_body)
}

-- Afficher le résultat
if code == 200 then
  print(table.concat(response_body))
else
  print("Erreur: " .. (code or "pas de réponse"))
end
```

Sortie:

```
<Les données récupérées depuis le serveur>
```

## Exploration approfondie

Historiquement, l'authentification HTTP de base a été introduite comme un moyen simple mais moins sécurisé pour contrôler l'accès aux ressources HTTP. Elle est simple à mettre en œuvre, mais à cause du faible niveau de sécurité, elle est souvent remplacée aujourd'hui par des méthodes plus sûres, comme OAuth.

En Lua, pour envoyer de telles requêtes, on utilise des bibliothèques comme `socket.http` et `mime` du module `LuaSocket`. Elles permettent de gérer la connexion réseau et l'encodage des identifiants respectivement. Notons que le mot de passe est simplement encodé en base64, ce qui n'est pas une forme de chiffrement résistante aux attaques.

Une alternative consiste à utiliser des bibliothèques HTTP plus avancées comme `luasec` qui supportent HTTPS, offrant ainsi une couche supplémentaire de sécurité grâce au chiffrement SSL/TLS.

## Voir aussi

- Documentation LuaSocket: http://w3.impa.br/~diego/software/luasocket/http.html
- LuaSec, pour un support HTTPS: https://github.com/brunoos/luasec/wiki
- RFC 7617, 'The 'Basic' HTTP Authentication Scheme': https://tools.ietf.org/html/rfc7617
