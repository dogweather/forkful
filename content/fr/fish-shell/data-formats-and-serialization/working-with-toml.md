---
date: 2024-01-26 04:21:20.878884-07:00
description: "TOML est un format de fichier de configuration, facile \xE0 lire et\
  \ \xE0 \xE9crire pour les humains, et facile \xE0 analyser et \xE0 g\xE9n\xE9rer\
  \ pour les machines. Les\u2026"
lastmod: 2024-02-19 22:05:16.999672
model: gpt-4-0125-preview
summary: "TOML est un format de fichier de configuration, facile \xE0 lire et \xE0\
  \ \xE9crire pour les humains, et facile \xE0 analyser et \xE0 g\xE9n\xE9rer pour\
  \ les machines. Les\u2026"
title: Travailler avec TOML
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
TOML est un format de fichier de configuration, facile à lire et à écrire pour les humains, et facile à analyser et à générer pour les machines. Les programmeurs utilisent TOML pour des fichiers de configuration clairs et hiérarchiques dans les projets où la lisibilité est primordiale.

## Comment faire :
Pour lire et manipuler TOML dans Fish, vous pourriez utiliser un outil comme `yj`, qui peut convertir TOML en JSON. Voici comment :

```fish
# Installer yj via Fisher
fisher install jorgebucaran/yj

# Convertir TOML en JSON
echo 'title = "Exemple TOML"' | yj -tj

# Exemple de sortie
{"title":"Exemple TOML"}
```

Pour écrire en TOML, vous inversez le processus :

```fish
# Convertir JSON en TOML
echo '{"title":"Exemple JSON"}' | yj -jt

# Exemple de sortie
title = "Exemple JSON"
```

Pour des tâches plus lourdes, envisagez un outil en ligne de commande TOML dédié comme `toml-cli`.

```fish
# Installer toml-cli
pip install toml-cli

# Définir une valeur dans un fichier TOML
toml set pyproject.toml tool.poetry.version "1.1.4"

# Obtenir une valeur d’un fichier TOML
set version (toml get pyproject.toml tool.poetry.version)
echo $version
```

## Plongée profonde
TOML (Tom's Obvious, Minimal Language), introduit par Tom Preston-Werner en 2013, est similaire à INI mais avec une spécification définie et une hiérarchie de données. Les principales alternatives sont JSON et YAML, mais elles ont leurs compromis : JSON n'est pas aussi convivial pour l'humain, tandis que YAML est plus complexe. La conception de TOML prospère dans des scénarios où les fichiers de configuration sont souvent maintenus à la main, équilibrant la simplicité et l'expressivité. En termes d'implémentation, des parseurs TOML sont disponibles pour la plupart des langages de programmation, y compris TomlBombadil pour Fish qui peut s'intégrer directement dans vos scripts.

## Voir aussi
- Spécification officielle de TOML : https://toml.io
- `yj`, un outil pour convertir entre TOML, JSON, YAML et XML : https://github.com/jorgebucaran/yj
- `toml-cli`, un utilitaire en ligne de commande pour TOML : https://github.com/sdispater/toml-cli
