---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:45.687361-07:00
description: "Comment faire : En Lua, l'\xE9criture dans stderr peut \xEAtre r\xE9\
  alis\xE9e en utilisant la fonction `io.stderr:write()`. Voici comment vous pouvez\
  \ \xE9crire un simple\u2026"
lastmod: '2024-03-13T22:44:57.955226-06:00'
model: gpt-4-0125-preview
summary: "En Lua, l'\xE9criture dans stderr peut \xEAtre r\xE9alis\xE9e en utilisant\
  \ la fonction `io.stderr:write()`."
title: "\xC9crire sur l'erreur standard"
weight: 25
---

## Comment faire :
En Lua, l'écriture dans stderr peut être réalisée en utilisant la fonction `io.stderr:write()`. Voici comment vous pouvez écrire un simple message d'erreur dans l'erreur standard :

```lua
io.stderr:write("Erreur : Entrée invalide.\n")
```

Si vous avez besoin de sortir une variable ou de combiner plusieurs morceaux de données, concaténez-les à l'intérieur de la fonction d'écriture :

```lua
local errorMessage = "Entrée invalide."
io.stderr:write("Erreur : " .. errorMessage .. "\n")
```

**Exemple de sortie sur stderr :**
```
Erreur : Entrée invalide.
```

Pour des scénarios plus complexes, ou lorsqu'on travaille avec des applications plus larges, vous pourriez envisager des bibliothèques de journalisation tierces telles que LuaLogging. Avec LuaLogging, vous pouvez diriger les journaux vers différentes destinations, y compris stderr. Voici un bref exemple :

D'abord, assurez-vous que LuaLogging est installé en utilisant LuaRocks :

```
luarocks install lualogging
```

Ensuite, pour écrire un message d'erreur dans stderr en utilisant LuaLogging :

```lua
local logging = require("logging")
local logger = logging.stderr()
logger:error("Erreur : Entrée invalide.")
```

Cette approche offre l'avantage d'une journalisation standardisée à travers votre application, avec la flexibilité ajoutée de définir les niveaux de log (par exemple, ERREUR, AVERTISSEMENT, INFO) grâce à une API simple.
