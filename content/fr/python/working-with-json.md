---
title:                "Travailler avec JSON"
html_title:           "Python: Travailler avec JSON"
simple_title:         "Travailler avec JSON"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/working-with-json.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi le faire ?

Travailler avec JSON est souvent une tâche courante pour les programmeurs en Python. JSON (JavaScript Object Notation) est un format de données léger et facile à lire qui est utilisé pour stocker et échanger des informations entre différentes applications. Les programmeurs utilisent souvent JSON pour stocker des données structurées telles que des objets et des tableaux dans des fichiers ou pour les envoyer sur un réseau.

# Comment faire :

```python
# Importer le module JSON
import json

# Créer un dictionnaire
mon_dictionnaire = {"langage": "python", "version": 3.9, "outil": "VSCode"}

# Convertir le dictionnaire en JSON
json_data = json.dumps(mon_dictionnaire)

# Afficher le JSON
print(json_data)
```

Output: {"langage": "python", "version": 3.9, "outil": "VSCode"}

# Plongée en profondeur :

## Contexte historique :
JSON a été créé en 2001 par Douglas Crockford et est devenu rapidement populaire en raison de sa simplicité et de sa compatibilité avec le langage JavaScript. Depuis lors, il est devenu un format de données standard dans de nombreuses langues de programmation, y compris Python.

## Alternatives :
Il existe d'autres formats de données similaires à JSON, tels que XML (Extensible Markup Language) ou YAML (Yet Another Markup Language). Cependant, JSON est préféré pour sa lisibilité, sa simplicité et sa compatibilité avec la plupart des langages de programmation.

## Détails d'implémentation :
Python dispose d'un module standard, json, qui prend en charge la manipulation de données JSON. Ce module offre des fonctions utiles telles que json.dumps() pour convertir des objets Python en JSON et json.loads() pour convertir du JSON en objets Python. De plus, il est également possible d'utiliser des bibliothèques externes telles que simplejson pour des fonctionnalités supplémentaires.

# Voir aussi :

- [Documentation officielle de Python sur JSON](https://docs.python.org/fr/3.9/library/json.html)
- [Documentation officielle de JSON](https://www.json.org/json-fr.html)
- [Tutoriel sur la manipulation de JSON avec Python](https://python.developpez.com/cours/TutoSwinnen/?page=I-6)