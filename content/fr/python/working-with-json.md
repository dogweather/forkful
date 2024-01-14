---
title:                "Python: Travailler avec JSON"
simple_title:         "Travailler avec JSON"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/working-with-json.md"
---

{{< edit_this_page >}}

# Pourquoi travailler avec JSON en Python ?

JSON (JavaScript Object Notation) est un format de données populaire pour échanger des informations entre différentes applications. En utilisant Python pour travailler avec JSON, vous pouvez facilement lire et écrire des données dans ce format et ainsi faciliter la communication entre vos différentes applications.

## Comment travailler avec JSON en Python

Pour commencer à travailler avec JSON en Python, vous devez d'abord importer le module `json` :

```Python
import json
```

### Lire des données JSON

Pour lire des données JSON à partir d'un fichier externe, vous pouvez utiliser la méthode`load()` du module `json` :

```Python
# Ouvrir le fichier JSON en lecture
with open("donnees.json") as f:
    # Charger les données JSON dans une variable
    data = json.load(f)

# Afficher les données
print(data)
```

Vous pouvez également utiliser cette méthode pour lire des données JSON à partir d'une chaîne de caractères :

```Python
# Exemple de données JSON en chaîne de caractères
donnees = '{"nom": "Jean", "age": 25, "ville": "Paris"}'

# Charger les données JSON dans une variable
data = json.loads(donnees)

# Afficher les données
print(data)
```

### Ecrire des données JSON

Pour écrire des données dans un fichier JSON, vous pouvez utiliser la méthode `dump()` du module `json` :

```Python
# Créer un dictionnaire avec des données
donnees = {
    "nom": "Marie",
    "age": 30,
    "ville": "Marseille"
}

# Ecrire les données dans un fichier JSON
with open("donnees.json", "w") as f:
    json.dump(donnees, f)
```

## Plongée dans l'utilisation de JSON en Python

Assurez-vous de vérifier la structure de vos données avant de les lire ou les écrire. Pour cela, vous pouvez utiliser la méthode `dumps()` pour convertir vos données en une chaîne de caractères et ainsi les afficher de manière plus lisible :

```Python
# Exemple de données JSON en dictionnaire
donnees = {
    "nom": "Pierre",
    "age": 35,
    "ville": "Lyon"
}

# Afficher les données sous forme de chaîne de caractères
print(json.dumps(donnees, indent=4))
```

Le paramètre `indent` permet de spécifier le nombre de caractères à utiliser pour l'indentation, ce qui rend les données plus faciles à lire.

De plus, notez que vous pouvez également travailler avec des données JSON imbriquées en utilisant des dictionnaires et des listes :

```Python
# Exemple de données JSON avec une liste imbriquée
donnees = {
    "clients": [
        {
            "nom": "Sophie",
            "age": 28,
            "ville": "Bordeaux"
        },
        {
            "nom": "Luc",
            "age": 33,
            "ville": "Toulouse"
        }
    ]
}

# Afficher les données
print(json.dumps(donnees, indent=4))
```

## Voir aussi

- [Documentation officielle de json en Python](https://docs.python.org/fr/3/library/json.html)
- [Guide pour créer et lire des fichiers JSON avec Python](https://www.geeksforgeeks.org/reading-and-writing-json-to-a-file-in-python/)