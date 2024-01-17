---
title:                "Travailler avec les CSV"
html_title:           "Python: Travailler avec les CSV"
simple_title:         "Travailler avec les CSV"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/working-with-csv.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?
Travailler avec les fichiers CSV en Python est un moyen simple et pratique de stocker, lire, et traiter des données tabulaires. Les programmeurs utilisent souvent ce format de fichier car il est facilement lisible par les humains et les machines, et peut être facilement manipulé dans Python.

## Comment faire:
Voici un exemple simple de lecture d'un fichier CSV en Python et d'affichage de son contenu:
```Python
import csv

# Ouverture du fichier CSV en mode lecture
with open('employes.csv', 'r') as csv_file:
    # Création d'un object lecteur CSV
    csv_reader = csv.reader(csv_file)

    # Boucle à travers les lignes du fichier
    for ligne in csv_reader:
        # Accès aux colonnes de chaque ligne
        nom = ligne[0]
        age = ligne[1]
        poste = ligne[2]

        # Affichage des informations
        print(f"Nom: {nom}, Age: {age}, Poste: {poste}")
```

Output:
```
Nom: Jean Dupont, Age: 30, Poste: Développeur
Nom: Marie Martin, Age: 25, Poste: Designer
Nom: Pierre Laurent, Age: 35, Poste: Analyste
```

## Plongée en profondeur:
Les fichiers CSV (pour Comma-Separated Values) ont été créés dans les années 1970 pour stocker des données dans des tableaux simple. Ils sont très populaires dans l'informatique car ils sont universellement compatibles et faciles à manipuler. Alternativement, vous pouvez également utiliser un format de fichier comme JSON ou XML pour stocker des données tabulaires en Python. Pour travailler avec des fichiers CSV, Python propose le module csv qui fournit des outils pour lire et écrire dans ces fichiers de manière efficace.

## À voir aussi:
- [Documentation officielle de Python sur le module csv](https://docs.python.org/fr/3/library/csv.html)
- [Un tutoriel sur les fichiers CSV en Python](https://realpython.com/python-csv/)