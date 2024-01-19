---
title:                "Vérifier si un répertoire existe"
html_title:           "Haskell: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Qu'est-ce & Pourquoi?
Vérifier qu'un répertoire existe est une étape où un programmeur demande au système de confirmer qu'un dossier spécifié est présent sur le système de fichiers. En le faisant, les programmeurs évitent d'essayer d'accéder à un dossier qui n'existe pas, ce qui permet d'éviter des erreurs gênantes.

## Comment faire :
L'un des moyens les plus simples de faire cela avec Python utilise les bibliothèques `os` et `os.path`. Voici un exemple :

```Python
import os

def est_existant(chemin):
    return os.path.isdir(chemin)

# Utilisez cette fonction pour vérifier l'existence d'un répertoire
print(est_existant("/chemin/vers/le/dossier"))
```
Si le répertoire existe, cela renverra `True`. Si ce n'est pas le cas, ça retournera `False`.

## Plongée profonde
Historiquement, vérifier qu'un répertoire existe n'était pas toujours facile. Avant la version actuelle de Python, nous avions besoin de gérer les exceptions, ce qui pouvait rendre le code plus difficile à lire et à déboguer.

Il existe aussi des alternatives comme l'utilisation de la bibliothèque `pathlib` disponible à partir de Python 3.4, comme ceci :

```Python
from pathlib import Path

def est_existant(chemin):
    return Path(chemin).is_dir()
```
Cette bibliothèque possède une compatibilité inter-plateformes et offre une interface orientée objet.

Au niveau de l'implémentation, vérifier si un répertoire existe revient fondamentalement à demander au système d'exploitation. Les détails précis dépendent de la manière dont le système d'exploitation et le système de fichiers gèrent les informations sur le répertoire.

## Voir aussi

Pour en savoir plus, voici quelques ressources utiles :
1. [Python os.path documentation](https://docs.python.org/3/library/os.path.html)
2. [Python pathlib documentation](https://docs.python.org/3/library/pathlib.html)
3. [StackOverflow - Check if a directory exists and create it if necessary](https://stackoverflow.com/questions/273192/check-if-a-directory-exists-and-create-it-if-necessary)