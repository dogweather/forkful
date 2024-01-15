---
title:                "Travailler avec les fichiers csv"
html_title:           "Python: Travailler avec les fichiers csv"
simple_title:         "Travailler avec les fichiers csv"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/working-with-csv.md"
---

{{< edit_this_page >}}

## Pourquoi
CSV (Comma-Separated Values) est un format de fichier couramment utilisé pour stocker des données tabulaires. Que vous soyez un étudiant en sciences de données, un développeur de logiciels ou un journaliste d'investigation, comprendre comment travailler avec des fichiers CSV peut être un outil extrêmement utile dans votre boîte à outils de compétences en informatique. Dans cet article, nous allons jeter un coup d'œil rapide à pourquoi et comment travailler avec des fichiers CSV en utilisant Python.

## Comment faire
Pour commencer, vous aurez besoin d'importer le module CSV dans votre script Python. Voici un exemple de code pour lire un fichier CSV et afficher son contenu :

```Python
import csv

with open('mon_fichier.csv', 'r') as fichier:
    lecteur = csv.reader(fichier)
    for ligne in lecteur:
        print(ligne)
```

Vous remarquerez que nous avons utilisé la boucle "for" pour parcourir chaque ligne du fichier et afficher son contenu. Vous pouvez également utiliser la méthode "next()" pour accéder à la première ligne du fichier si vous avez des en-têtes de colonne.

Si vous souhaitez écrire dans un fichier CSV, vous pouvez utiliser le module "csv.writer" et la méthode "writerow()" pour écrire chaque ligne dans le fichier. Voici un exemple de code :

```Python
import csv

with open('nouveau_fichier.csv', 'w') as nouveau_fichier:
    ecrivain = csv.writer(nouveau_fichier)
    ecrivain.writerow(['Prénom', 'Nom', 'Âge'])
    ecrivain.writerow(['Jean', 'Dupont', '25'])
    ecrivain.writerow(['Marie', 'Dubois', '30'])
```

Ceci créera un nouveau fichier CSV avec trois colonnes et deux lignes de données.

## Plongée en profondeur
Il est important de noter que les fichiers CSV peuvent avoir différents délimiteurs de champ, pas seulement des virgules. Vous pouvez spécifier le délimiteur en utilisant l'argument "delimiter" dans les fonctions "csv.reader()" et "csv.writer()".

De plus, Python a un module intégré appelé "csv.DictReader" qui vous permet de lire un fichier CSV et d'accéder aux données en utilisant des noms de colonnes plutôt que des indices. Ceci est très utile si vous avez un grand nombre de colonnes dans votre fichier CSV.

Enfin, n'oubliez pas de fermer votre fichier après avoir fini de le manipuler en utilisant la méthode "close()". Vous pouvez également utiliser la clause "with" pour vous assurer que le fichier est automatiquement fermé une fois que vous avez terminé de travailler avec lui.

## Voir aussi
- Documentation officielle pour le module CSV de Python : https://docs.python.org/3/library/csv.html
- Des instructions supplémentaires pour travailler avec des fichiers CSV en Python : https://realpython.com/python-csv/