---
title:                "Vérification de l'existence d'un répertoire"
html_title:           "Python: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi vous devriez vous soucier de vérifier si un répertoire existe en Python. Et bien, cela peut être utile si vous voulez vous assurer qu'un répertoire existe avant de créer un nouveau fichier ou si vous avez besoin de parcourir tous les fichiers dans un répertoire spécifique.

## Comment faire

La vérification de l'existence d'un répertoire en Python est assez simple. Tout d'abord, vous devez importer le module `os` qui contient des fonctions pour interagir avec le système d'exploitation. Ensuite, vous pouvez utiliser la fonction `os.path.exists()` en lui passant le chemin du répertoire que vous souhaitez vérifier comme argument.

```python
import os

# Vérifie si le répertoire "documents" existe dans le répertoire actuel
if os.path.exists("documents"):
    print("Le répertoire existe!")
else:
    print("Le répertoire n'existe pas!")
```

Si le répertoire existe, la fonction renvoie `True` et `False` sinon. Vous pouvez également utiliser la fonction `os.path.isdir()` pour vérifier si un chemin donné correspond à un répertoire ou non.

```python
import os

path = "dossier/exemple"

# Vérifie si le chemin correspond à un répertoire
if os.path.isdir(path):
    print(f"Le chemin {path} correspond à un répertoire!")
else:
    print(f"Le chemin {path} ne correspond pas à un répertoire!")
```

## Plongée en profondeur

Pour comprendre comment la vérification de l'existence d'un répertoire fonctionne en Python, il est utile de connaître les différents types de chemins disponibles. Les chemins absolus commencent par la racine du système de fichiers, tandis que les chemins relatifs commencent par le répertoire actuel.

Il est également important de noter que les chemins peuvent varier en fonction du système d'exploitation. Par exemple, le chemin absolu pour le répertoire `documents` peut être `/home/user/documents` sur Linux et `C:\\Users\\User\\Documents` sur Windows.

## Voir aussi

- [Documentation officielle Python sur le module os](https://docs.python.org/fr/3/library/os.html)
- [Guide pratique sur la manipulation des fichiers et répertoires en Python](https://realpython.com/working-with-files-in-python/)
- [Tutoriel en français sur l'utilisation du module os en Python](https://python.doctor/page-python-chemin-dossier-existe-os-path-exists)

En conclusion, la vérification de l'existence d'un répertoire en Python peut être utile dans de nombreuses situations et il est important de connaître les différentes fonctions et méthodes disponibles pour y parvenir. N'hésitez pas à explorer davantage le module `os` et à essayer différents exemples pour mieux comprendre son fonctionnement.