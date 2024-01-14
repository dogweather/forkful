---
title:                "Python: Écrire un fichier texte"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Écrire un fichier texte est une tâche utile pour tout programmeur Python. Cela vous permet de stocker des données de manière structurée et de les récupérer facilement pour une utilisation ultérieure.

## Comment faire

Il existe plusieurs façons d'écrire un fichier texte en Python, mais voici une méthode simple qui utilise la fonction intégrée open() :

```Python
fichier = open("mon_fichier.txt", "w")
fichier.write("Bonjour tout le monde, c'est moi !")
fichier.close()
```

Ce code ouvre un fichier nommé "mon_fichier.txt" en mode écriture ("w") et y écrit la phrase "Bonjour tout le monde, c'est moi !". N'oubliez pas de fermer le fichier avec la méthode close() après avoir fini d'écrire.

## Plongée en profondeur

La fonction open() accepte un deuxième argument optionnel pour spécifier le mode d'ouverture du fichier : "r" pour lecture seule, "a" pour ajout et "x" pour créer un nouveau fichier et écrire dedans. Vous pouvez également spécifier le mode "b" pour ouvrir le fichier en mode binaire.

Outre la méthode write(), le fichier dispose également de méthodes telles que read() pour lire le contenu du fichier et readline() pour lire une ligne spécifique. Vous pouvez également utiliser une boucle for pour parcourir toutes les lignes du fichier.

## Voir aussi

- [Documentation officielle de Python pour l'écriture de fichiers](https://docs.python.org/fr/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Tutoriel vidéo sur l'écriture de fichiers en Python](https://www.youtube.com/watch?v=Uh2ebFW8OYM)
- [Exemples de projets utilisant l'écriture de fichiers en Python](https://www.upgrad.com/blog/python-file-handling-lets-write-a-file-parsing-program/)

Merci d'avoir lu cet article sur l'écriture de fichiers en Python ! Nous espérons que cela vous a été utile dans vos projets de programmation. N'hésitez pas à utiliser ces connaissances pour créer vos propres fichiers textes dans vos futurs scripts Python.