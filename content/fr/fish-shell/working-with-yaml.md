---
title:                "Travailler avec YAML"
date:                  2024-01-19
html_title:           "Bash: Travailler avec YAML"
simple_title:         "Travailler avec YAML"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
YAML est un format de données simplifié, idéal pour les fichiers de config. Les programmeurs l'utilisent pour sa lisibilité et sa simplicité.

## Comment faire :
```Fish Shell
# Installer 'yq', une commande pour parser YAML
sudo apt install yq

# Lire une valeur
echo 'name: Fish Shell' | yq e '.name' -

# Sortie : Fish Shell

# Modifier une valeur
echo 'name: Old Name' | yq e '.name = "New Name"' -

# Sortie : name: New Name

# Convertir JSON en YAML
echo '{"name": "Fish Shell"}' | yq e -P -

# Sortie :
# name: Fish Shell
```

## Plongée en profondeur
YAML, pour "YAML Ain't Markup Language", est né en 2001. Son but : la simplicité, contrairement à XML, plus complexe. Le choix se porte souvent sur YAML pour configurer des applications, des environnements de dev, etc. En Fish Shell, 'yq' est l'outil le plus répandu, mais il existe aussi 'yaml-cpp' et 'PyYAML' pour d'autres langages.

## Voir aussi
- Documentation officielle de YAML : https://yaml.org/spec/1.2/spec.html
- Repo GitHub de 'yq' : https://github.com/mikefarah/yq
- Tutoriel YAML : https://www.tutorialspoint.com/yaml/index.htm
