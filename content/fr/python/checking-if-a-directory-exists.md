---
title:                "Python: Vérification de l'existence d'un répertoire"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi vous devriez vérifier si un répertoire existe en Python ? La réponse est simple : cela peut être utile lors de la gestion de fichiers et de dossiers dans vos programmes. En vérifiant si un répertoire existe avant de l'ouvrir ou de le créer, vous évitez les erreurs et les conflits.

## Comment faire

Pour vérifier si un répertoire existe en Python, vous pouvez utiliser la fonction `os.path.exists()` de la bibliothèque standard `os`. Cette fonction prend en argument le chemin du répertoire que vous souhaitez vérifier et renvoie une valeur booléenne `True` si le répertoire existe, ou `False` dans le cas contraire.

```python
import os

if os.path.exists('chemin/du/répertoire'): # Remplacez "chemin/du/répertoire" par le chemin réel de votre répertoire
    print("Le répertoire existe !")
else:
    print("Le répertoire n'existe pas.")
```

Vous pouvez également utiliser la fonction `os.path.isdir()` pour vérifier si un chemin donné correspond à un répertoire existant.

```python
import os

if os.path.isdir('chemin/du/répertoire'):
    print("Ceci est bien un répertoire.")
else:
    print("Ce n'est pas un répertoire.")
```

## Approfondissement

Maintenant que vous savez comment vérifier si un répertoire existe en Python, vous pouvez aller plus loin et apprendre à créer des répertoires avec `os.mkdir()` ou à naviguer dans l'arborescence des répertoires avec `os.chdir()`. Vous pouvez également vous renseigner sur les exceptions qui peuvent être levées lors de la gestion des répertoires et comment les gérer efficacement dans votre code.

## Voir aussi

- [Documentation officielle de Python sur les opérations de fichiers/dossiers](https://docs.python.org/fr/3/library/filesys.html)
- [Tutoriel sur la gestion de fichiers/dossiers en Python](https://realpython.com/working-with-files-in-python/)
- [Article sur les exceptions en Python](https://realpython.com/python-exceptions/)