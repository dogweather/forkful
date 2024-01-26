---
title:                "Écrire dans l'erreur standard"
html_title:           "Arduino: Écrire dans l'erreur standard"
simple_title:         "Écrire dans l'erreur standard"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/lua/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Écrire sur l'erreur standard (stderr) permet d'envoyer des messages d'erreur ou des diagnostics séparément des données normales (stdout). Les programmeurs utilisent stderr pour signaler des problèmes sans perturber le flux principal des données.

## How to:
Lua simplifie l'écriture sur stderr. Utilisez `io.stderr:write()` pour envoyer des messages d'erreur.

```lua
-- Envoyer un message simple à stderr
io.stderr:write("Erreur trouvée!\n")

-- Exemple avec une condition
local fichier = io.open("fichier_inexistant.txt", "r")
if fichier == nil then
    io.stderr:write("Impossible d'ouvrir le fichier.\n")
end
```

Sortie possible sur stderr :
```
Erreur trouvée!
Impossible d'ouvrir le fichier.
```

## Deep Dive
Historiquement, séparer stdout et stderr permet aux systèmes Unix de rediriger les flux indépendamment. En Lua, `io.stderr` est un objet de fichier déjà ouvert, toujours disponible pour écrire les erreurs. Des alternatives comme le logging dans un fichier spécifique existent, mais l'usage de stderr est un standard sous Unix.

## See Also
- Documentation Lua pour `io.stderr`: http://www.lua.org/manual/5.4/manual.html#6.8
- Pourquoi stderr est utile : https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr)
- Guide Unix sur la redirection des flux : https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO-3.html
