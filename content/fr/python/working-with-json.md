---
title:                "Travailler avec json"
html_title:           "Python: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/working-with-json.md"
---

{{< edit_this_page >}}

## Pourquoi

JSON (JavaScript Object Notation) est un format de données populaire utilisé pour échanger des informations entre différentes plateformes et applications. Il est largement utilisé dans les applications web, les API et les bases de données. En apprenant à travailler avec JSON, vous serez en mesure de manipuler et échanger des données de manière efficace, ce qui est essentiel pour tout développeur.

## Comment faire

Pour travailler avec JSON en Python, vous devez utiliser le module intégré "json". Voici un exemple de code simple pour convertir des données JSON en un dictionnaire Python et accéder à ses valeurs:

```
import json

# Exemple de données JSON
json_data = '{"pays": "France", "ville": "Paris", "population": 2.161}'

# Conversion en dictionnaire Python
data_dict = json.loads(json_data)

# Accès aux valeurs
print(data_dict["pays"]) # Output: France
print(data_dict["population"]) # Output: 2.161

```

Vous pouvez également utiliser la méthode "json.dumps()" pour convertir un dictionnaire Python en format JSON. Voici un exemple:

```
import json

# Exemple de dictionnaire Python
data_dict = {"nom": "Sophie", "âge": 27, "statut": "Célibataire"}

# Conversion en JSON
json_data = json.dumps(data_dict)

print(json_data) # Output: {"nom": "Sophie", "âge": 27, "statut": "Célibataire"}
```

## Plongée en profondeur

Le module "json" offre également des fonctionnalités avancées pour travailler avec des fichiers JSON volumineux et complexes. Vous pouvez utiliser des paramètres tels que "indent" pour formater le JSON en une structure facilement lisible ou "sort_keys" pour trier les clés du dictionnaire en ordre alphabétique. De plus, le module "json" offre des méthodes pour lire et écrire des fichiers JSON directement dans votre code Python.

Pour en savoir plus sur les fonctionnalités avancées et les bonnes pratiques pour travailler avec JSON en Python, consultez la documentation officielle de Python ainsi que des ressources en ligne telles que "Real Python".

## Voir aussi

- Documentation officielle de Python sur le module "json": https://docs.python.org/fr/3/library/json.html
- Tutoriel "Real Python" sur la manipulation de JSON en Python: https://realpython.com/python-json/