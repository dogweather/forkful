---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:08.129720-07:00
description: "Comment faire : Lua n'int\xE8gre pas de support pour YAML, mais vous\
  \ pouvez travailler avec des fichiers YAML en utilisant des biblioth\xE8ques tierces\
  \ telles\u2026"
lastmod: '2024-03-13T22:44:57.960397-06:00'
model: gpt-4-0125-preview
summary: "Lua n'int\xE8gre pas de support pour YAML, mais vous pouvez travailler avec\
  \ des fichiers YAML en utilisant des biblioth\xE8ques tierces telles que `lyaml`."
title: Travailler avec YAML
weight: 41
---

## Comment faire :
Lua n'intègre pas de support pour YAML, mais vous pouvez travailler avec des fichiers YAML en utilisant des bibliothèques tierces telles que `lyaml`. Cette bibliothèque permet de coder et décoder des données YAML avec Lua. Tout d'abord, vous devrez installer `lyaml` via LuaRocks, le gestionnaire de paquets de Lua :

```bash
luarocks install lyaml
```

### Décodage de YAML :
Supposons que vous ayez le contenu YAML suivant dans un fichier nommé `config.yaml` :

```yaml
database:
  host: localhost
  port: 3306
  username: utilisateur
  password: motdepasse
```

Vous pouvez décoder ce fichier YAML en une table Lua avec le code suivant :

```lua
local yaml = require('lyaml')
local file = io.open("config.yaml", "r")
local content = file:read("*all")
file:close()

local data = yaml.load(content)
for k,v in pairs(data.database) do
  print(k .. ": " .. v)
end
```

Lorsque vous exécutez ce script, il devrait afficher :

```output
host: localhost
port: 3306
username: utilisateur
password: motdepasse
```

### Encodage en YAML :
Pour encoder des tables Lua au format YAML, vous utilisez la fonction `dump` fournie par `lyaml`. Considérant que vous souhaitez créer une représentation YAML de la table Lua suivante :

```lua
local data = {
  website = {
    name = "Exemple",
    owner = "Jane Doe",
    metadata = {
      creation_date = "2023-01-01",
      tags = {"blog", "personnel", "lua"}
    }
  }
}

local yaml = require('lyaml')
local yaml_data = yaml.dump({data})
print(yaml_data)
```

Le YAML de sortie sera :

```yaml
- website:
    metadata:
      creation_date: '2023-01-01'
      tags: [blog, personnel, lua]
    name: Exemple
    owner: Jane Doe
```

En suivant ces modèles, les programmeurs Lua peuvent gérer efficacement les données YAML pour une variété d'applications. Ces opérations avec YAML sont cruciales pour développer des applications Lua polyvalentes qui interagissent en douceur avec d'autres parties d'un système ou directement avec d'autres systèmes.
