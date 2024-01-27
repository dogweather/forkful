---
title:                "Travailler avec YAML"
date:                  2024-01-19
html_title:           "Bash: Travailler avec YAML"
simple_title:         "Travailler avec YAML"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
Le YAML est un format de données facile à lire. Les programmeurs l'utilisent pour la configuration, l'orchestration de déploiement et le stockage de données simples.

## How to:
Installer `yq`, un outil pour manipuler YAML en ligne de commande :
```Bash
sudo wget https://github.com/mikefarah/yq/releases/download/v4.25.1/yq_linux_amd64 -O /usr/bin/yq && sudo chmod +x /usr/bin/yq
```

Lire une valeur :
```Bash
echo "nom: Dupont" | yq eval '.nom' -
# Output: Dupont
```

Modifier une valeur :
```Bash
echo -e "nom: Dupont\nage: 42" | yq eval '.age = 43' -
# Output:
# nom: Dupont
# age: 43
```

Ajouter un élément à une liste :
```Bash
echo -e "fruits:\n  - pomme" | yq eval '.fruits += ["banane"]' -
# Output:
# fruits:
#   - pomme
#   - banane
```

## Deep Dive
YAML est né en 2001, pour la simplicité contre XML. Alternatives : JSON (moins lisible), TOML (plus simple). `Yq` utilise `libyaml` pour la précision et respecte la spécification YAML.

## See Also
Documentation `yq`: https://mikefarah.gitbook.io/yq/
Spécification YAML: https://yaml.org/spec/1.2/spec.html
Comparaison YAML/JSON/TOML: https://blog.boltflare.com/yaml-vs-json-vs-toml-which-is-the-best-configuration-language/
