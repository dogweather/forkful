---
title:                "Travailler avec le yaml"
html_title:           "Python: Travailler avec le yaml"
simple_title:         "Travailler avec le yaml"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/working-with-yaml.md"
---

{{< edit_this_page >}}

# Qu'est-ce que YAML et pourquoi les programmeurs l'utilisent-ils?

YAML est un langage de sérialisation de données qui permet de stocker des données structurées de manière lisible pour les humains. Les programmeurs utilisent YAML pour stocker des configurations, des données de test et d'autres informations dans un format facile à lire et à éditer.

# Comment faire:

Pour travailler avec YAML en Python, vous devez d'abord importer le module PyYAML en utilisant la commande `import yaml`. Ensuite, vous pouvez utiliser les fonctions de chargement et de déchargement pour convertir des données YAML en objets Python et vice versa.

Un exemple de code pour charger des données YAML à partir d'un fichier serait le suivant:

```python
import yaml

with open("config.yml", 'r') as f:
    config = yaml.safe_load(f)

print(config)
```

Et voici comment vous pouvez enregistrer des données en tant que fichier YAML:

```python
import yaml

config = {'nom': 'John', 'age': 30, 'adresse': '123 Rue du Village'}
with open("config.yml", 'w') as f:
    yaml.safe_dump(config, f)
```

Cela produirait un fichier YAML contenant les données suivantes:

```
nom: John
age: 30
adresse: 123 Rue du Village
```

# Plongée en profondeur:

YAML a été créé en 2001 par Clark Evans, Ingy döt Net et Oren Ben-Kiki en tant que langage de sérialisation de données plus simple et plus intuitif que d'autres formats comme XML et JSON. Il est également inspiré de formats de données tels que le courrier électronique, le HTML et le SGML.

Bien que YAML soit couramment utilisé pour stocker des configurations, il existe d'autres alternatives telles que TOML et JSON. YAML se distingue par sa simplicité et sa lisibilité pour les humains, mais cela peut entraîner des problèmes lors de la manipulation de données plus complexes.

L'implémentation de YAML utilisée par PyYAML est basée sur la spécification officielle YAML 1.2. Cependant, cela peut entraîner des problèmes de compatibilité avec certaines autres implémentations de YAML.

# Voir aussi:

- Site officiel de YAML: https://yaml.org/
- Documentation de PyYAML: https://pyyaml.org/wiki/PyYAMLDocumentation
- Tutoriel sur l'utilisation de YAML avec Python: https://realpython.com/python-yaml/
- Comparaison entre YAML, JSON et TOML: https://stackabuse.com/an-introduction-to-yaml/