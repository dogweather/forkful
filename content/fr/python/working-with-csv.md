---
title:                "Manipulation des fichiers CSV"
date:                  2024-01-19
html_title:           "Bash: Manipulation des fichiers CSV"
simple_title:         "Manipulation des fichiers CSV"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/working-with-csv.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Manipuler des fichiers CSV c’est gérer des données tabulaires, comme un Excel mais en plus simple. On bosse dessus pour importer, exporter, et analyser des données facilement entre des systèmes et des applis.

## Comment faire :
```Python
import csv

# Lire un fichier CSV
with open('exemplaire.csv', 'r') as fichier:
    lecteur = csv.reader(fichier)
    for ligne in lecteur:
        print(ligne)

# Écrire dans un fichier CSV
with open('exemplaire_save.csv', 'w', newline='') as fichier:
    ecrivain = csv.writer(fichier)
    ecrivain.writerow(["nom", "ville", "age"])
    ecrivain.writerow(["Alex", "Paris", "30"])
```

Sortie:
```
['nom', 'ville', 'age']
['Alex', 'Paris', '30']
```

## Plongée Profonde
CSV, ça vient des années 70, simple et universel. Alternatives? JSON, XML, mais CSV reste roi pour la compatibilité. En Python, le module `csv` est standard mais pour du lourd, Pandas gère mieux les gros datasets.

## À Voir Aussi
- La doc Python sur CSV: https://docs.python.org/3/library/csv.html
- Tutoriel Pandas: https://pandas.pydata.org/pandas-docs/stable/user_guide/io.html
- Comparaison de formats de données: https://en.wikipedia.org/wiki/Comparison_of_data_serialization_formats
