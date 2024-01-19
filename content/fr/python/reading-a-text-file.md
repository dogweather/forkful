---
title:                "Lecture d'un fichier texte"
html_title:           "Arduino: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
La lecture d'un fichier texte en Python signifie récupérer les données d'un fichier texte pour une utilisation dans un programme. Les programmeurs font cela pour manipuler des données, construire des modèles ou effectuer des analyses.

## Comment faire :
La lecture d'un fichier texte en Python est un processus simple. Voici comment elle est effectuée :

```Python
# Ouverture d'un fichier
fichier = open('fichier.txt', 'r')
  
# Lecture du fichier
print(fichier.read())
  
# Fermeture du fichier
fichier.close()
```

Example de sortie:

```Python
Bonjour, monde !
```

## Plongée en Profondeur
L'ouverture de fichiers en Python remonte aux premières versions du langage. Au fil des ans, il est devenu de plus en plus facile de le faire. 

En termes d'alternatives, vous pouvez également utiliser la méthode `with open`, qui ferme le fichier automatiquement une fois que vous avez terminé.

```Python
with open('fichier.txt', 'r') as fichier:
    print(fichier.read())
```

Concernant les détails d'implémentation, lorsque vous ouvrez un fichier avec `open()`, Python crée un objet fichier. Cet objet possède plusieurs méthodes de lecture de fichiers à votre disposition, comme `read()`, `readline()`, ou `readlines()` pour lire tout le fichier, lire une ligne, ou lire toutes les lignes dans une liste, respectivement.

## Voir Aussi
Pour plus d'informations, consultez ces liens utiles:
- [Documentation Officielle Python sur les opérations de fichiers](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Tutorial Python sur GeeksforGeeks](https://www.geeksforgeeks.org/file-handling-python/)
- [Guide sur Real Python](https://realpython.com/read-write-files-python/)