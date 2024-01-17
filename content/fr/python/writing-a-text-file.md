---
title:                "Écrire un fichier texte"
html_title:           "Python: Écrire un fichier texte"
simple_title:         "Écrire un fichier texte"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?
Écrire un fichier texte en informatique signifie enregistrer des données dans un format lisible par les humains. Les programmeurs le font souvent pour stocker des informations ou des paramètres pour leurs programmes.

## Comment faire:
```Python
# Étape 1 : Ouvrir le fichier en mode écriture avec la fonction "open"
fichier = open("mon_fichier.txt", "w")

# Étape 2 : Écrire les données dans le fichier avec la méthode "write"
fichier.write("Voici un exemple de données que nous enregistrerons dans notre fichier.")

# Étape 3 : Fermer le fichier pour libérer la mémoire avec la méthode "close"
fichier.close()
```
Voici le contenu qui sera enregistré dans notre fichier texte:
```
Voici un exemple de données que nous enregistrerons dans notre fichier.
```

## Plongée en profondeur:
Écrire un fichier texte peut sembler un concept simple, mais c'est un élément fondamental de la programmation. Avant l'avènement des bases de données, les fichiers texte étaient le moyen le plus courant de stocker des données. Il existe également différentes manières d'écrire dans un fichier texte, comme utiliser des bibliothèques spécifiques ou écrire en utilisant différentes méthodes.

## À voir également:
Pour en savoir plus sur l'écriture de fichiers texte en Python, vous pouvez consulter la documentation officielle ici: https://docs.python.org/fr/3/tutorial/inputoutput.html#lire-et-%c3%a9crire-des-fichiers