---
title:                "Python: Travailler avec des fichiers csv"
simple_title:         "Travailler avec des fichiers csv"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/working-with-csv.md"
---

{{< edit_this_page >}}

##Pourquoi
Les fichiers CSV (comma-separated values) sont couramment utilisés pour stocker et échanger des données, en particulier dans le domaine de la science des données. Savoir travailler avec des fichiers CSV est donc une compétence précieuse à maîtriser pour tout développeur en Python.

##Comment faire
Pour commencer à travailler avec des fichiers CSV en Python, nous avons besoin d'importer le module `csv`. Voici un exemple de code qui lit un fichier CSV et affiche son contenu :

```python
import csv

with open('donnees.csv', 'r') as f:
    lecteur_csv = csv.reader(f)
    for ligne in lecteur_csv:
        print(ligne)
```

La sortie de ce code sera un ensemble de listes, chaque liste représentant une ligne du fichier CSV. Par exemple, si notre fichier CSV contient les données suivantes :
```
nom,prenom,age
Dupont,Jean,25
Martin,Lucie,34
```
La sortie du code ci-dessus sera :
```
['nom', 'prenom', 'age']
['Dupont', 'Jean', '25']
['Martin', 'Lucie', '34']
```
Il est important de noter que les valeurs dans les fichiers CSV sont toujours des chaînes de caractères, même si elles représentent des nombres. Pour convertir les valeurs en nombres, nous pouvons utiliser la fonction `int()` ou `float()`.

##Plongée profonde
Si vous souhaitez en savoir plus sur la façon de travailler avec des fichiers CSV en Python, vous pouvez également utiliser le module `csv.DictReader` qui vous permet de lire le fichier CSV en tant que dictionnaires plutôt que des listes. Cela facilite l'accès à des valeurs spécifiques dans le fichier.

De plus, vous pouvez également utiliser le module `csv.writer` pour écrire des données dans un fichier CSV. Voici un exemple de code qui écrit une liste de listes dans un fichier CSV :

```python
import csv

donnees = [
    ['nom', 'prenom', 'age'],
    ['Dupont', 'Jean', 25],
    ['Martin', 'Lucie', 34]
]

with open('donnees.csv', 'w', newline='') as f:
    ecrivain_csv = csv.writer(f)
    for ligne in donnees:
        ecrivain_csv.writerow(ligne)
```

##Voir aussi
- [Documentation officielle de Python sur le module csv](https://docs.python.org/fr/3/library/csv.html)
- [Tutoriel sur les fichiers CSV en Python](https://www.datacamp.com/community/tutorials/working-with-csv-files-in-python)
- [Vidéo YouTube sur la manipulation de fichiers CSV en Python](https://www.youtube.com/watch?v=q5uM4VKywbA)