---
title:                "Lire un fichier texte"
html_title:           "Python: Lire un fichier texte"
simple_title:         "Lire un fichier texte"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur Python, il est fort probable que vous ayez besoin de lire un fichier texte à un moment donné. Cela peut être pour récupérer des données stockées, traiter des informations ou simplement vérifier le contenu d'un fichier texte. Dans tous les cas, savoir comment lire un fichier texte en Python peut être très utile.

## Comment faire

Il existe plusieurs façons de lire un fichier texte en Python, mais nous allons nous concentrer sur la méthode la plus couramment utilisée.

Tout d'abord, nous devons ouvrir le fichier en utilisant la fonction `open()`. Cette fonction prend deux paramètres, le nom du fichier et le mode d'ouverture. Le mode d'ouverture peut être "r" pour lecture seule, "w" pour écriture (qui écrasera le contenu existant) ou "a" pour ajout à la fin du fichier. Par défaut, le mode est défini sur "r", donc si vous voulez simplement lire le fichier, vous pouvez omettre ce paramètre.

```
fichier = open("fichier.txt")
```

Ensuite, nous allons utiliser la méthode `read()` pour lire le contenu du fichier et le stocker dans une variable.

```
donnees = fichier.read()
```

Enfin, n'oubliez pas de fermer le fichier avec la méthode `close()` pour éviter les problèmes de mémoire.

```
fichier.close()
```

Voici un exemple complet de la méthode pour lire un fichier texte et afficher son contenu :

```
fichier = open("fichier.txt")
donnees = fichier.read()
print(donnees)
fichier.close()
```

Et voici le résultat que nous pouvons obtenir si notre fichier texte contient le contenu suivant :

```
Bienvenue dans cet exemple de lecture de fichier texte en Python !
```

Output :

```
Bienvenue dans cet exemple de lecture de fichier texte en Python !
```

## Un peu plus en profondeur

Maintenant que vous connaissez les bases de la lecture d'un fichier texte en Python, il est important de noter que la méthode `read()` peut être utilisée de différentes manières. Par exemple, vous pouvez spécifier le nombre de caractères à lire en passant un argument à la méthode :

```
# Pour lire les 10 premiers caractères du fichier
donnees = fichier.read(10)
```

De plus, si vous préférez lire le fichier ligne par ligne, vous pouvez utiliser la méthode `readlines()` :

```
fichier = open("fichier.txt")
lignes = fichier.readlines()
for ligne in lignes:
  print(ligne)
fichier.close()
```

Il est également possible de spécifier un chemin d'accès absolu ou relatif pour ouvrir un fichier texte. Vous pouvez en savoir plus sur les chemins d'accès en consultant la [documentation officielle de Python](https://docs.python.org/fr/3/library/pathlib.html).

Enfin, n'oubliez pas que les fichiers ouverts doivent être fermés pour éviter les problèmes de mémoire et de performances. Vous pouvez utiliser la structure `with` pour ouvrir un fichier et vous n'aurez pas à vous soucier de le fermer :

```
with open("fichier.txt") as fichier:
  donnees = fichier.read()
```

## Voir aussi

Pour en savoir plus sur la lecture de fichiers en Python, vous pouvez consulter les ressources suivantes :

- [Documentation officielle de Python sur les fichiers](https://docs.python.org/fr/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Article "Working with Text Files in Python" de Real Python (en anglais)](https://realpython.com/read-write-files-python/)

Maintenant que vous savez comment lire un fichier texte en Python, vous pourrez utiliser cette compétence pour de nombreux projets futurs. Bonne programmation !