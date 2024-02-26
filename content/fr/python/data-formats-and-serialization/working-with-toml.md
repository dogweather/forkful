---
date: 2024-01-26 04:25:27.563379-07:00
description: "TOML, abr\xE9viation de Tom's Obvious, Minimal Language, est un format\
  \ de s\xE9rialisation de donn\xE9es comparable \xE0 JSON ou YAML, mais il vise la\
  \ simplicit\xE9 et\u2026"
lastmod: '2024-02-25T18:49:54.151037-07:00'
model: gpt-4-0125-preview
summary: "TOML, abr\xE9viation de Tom's Obvious, Minimal Language, est un format de\
  \ s\xE9rialisation de donn\xE9es comparable \xE0 JSON ou YAML, mais il vise la simplicit\xE9\
  \ et\u2026"
title: Travailler avec TOML
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
TOML, abréviation de Tom's Obvious, Minimal Language, est un format de sérialisation de données comparable à JSON ou YAML, mais il vise la simplicité et la lisibilité. Les programmeurs utilisent TOML pour les fichiers de configuration car il est facile à écrire et à comprendre, et il se cartographie proprement sur des structures de données dans des langages de programmation comme Python.

## Comment faire :
Avant de commencer, installez le paquet `toml` avec `pip install toml`. Analysons un fichier TOML :

```python
import toml

# Exemple de contenu TOML en chaîne de caractères
toml_string = """
[propriétaire]
nom = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z # Des dates de première classe

[base_de_données]
serveur = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
"""

# Analyser la chaîne TOML
parsed_toml = toml.loads(toml_string)

# Accéder aux données
print(parsed_toml['propriétaire']['nom'])  # Résultat : Tom Preston-Werner
print(parsed_toml['base_de_données']['ports'])  # Résultat : [8001, 8001, 8002]
```

## Plongée approfondie
TOML a été créé par Tom Preston-Werner, l'un des fondateurs de GitHub, comme un format de fichier de configuration plus convivial. Il est conçu pour se cartographier sans ambiguïté sur une table de hachage et être facilement analysable par les machines.

Comparé à JSON, TOML est plus lisible pour les fichiers de configuration et supporte les commentaires. YAML, une autre alternative, peut être plus compact, mais sa dépendance à l'indentation et les problèmes subtils, comme l'interdiction des tabulations, peuvent dérouter les gens.

En ce qui concerne les détails d'implémentation, les valeurs TOML sont typées, ce qui inclut les chaînes, les entiers, les flottants, les booléens, les dates, les tableaux et les tables. Tout est sensible à la casse. De plus, TOML prend en charge les chaînes sur plusieurs lignes et, dans la dernière version, permet même les tableaux de types hétérogènes.

Python utilise la bibliothèque `toml`, qui reflète les bibliothèques JSON et YAML en termes d'API. Vous avez `toml.load` et `toml.loads` pour lire TOML à partir d'un fichier ou d'une chaîne, respectivement, et `toml.dump` et `toml.dumps` pour l'écrire.

## Voir aussi
- Le dépôt GitHub officiel de TOML pour les spécifications : [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- La documentation de la bibliothèque Python `toml` : [pypi.org/project/toml/](https://pypi.org/project/toml/)
- Des exemples réels de TOML : Fichiers de configuration pour le gestionnaire de paquets de Rust `cargo` ou l'outil de packaging Python `poetry`.
