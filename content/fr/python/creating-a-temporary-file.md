---
title:                "Créer un fichier temporaire"
html_title:           "Python: Créer un fichier temporaire"
simple_title:         "Créer un fichier temporaire"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Créer un fichier temporaire est une pratique courante en programmation Python. Les programmeurs le font pour stocker temporairement des données ou des fichiers qui ne seront pas nécessaires après leur utilisation.

## Comment faire:
```Python
import tempfile

with tempfile.TemporaryFile() as tmp:
    # écrire des données dans le fichier temporaire
    tmp.write(b"Ceci est un exemple de données à écrire dans le fichier temporaire")
    # lire les données écrites
    tmp.seek(0)
    data = tmp.read()
    # afficher les données
    print(data)
```
Output: `b"Ceci est un exemple de données à écrire dans le fichier temporaire"`

## Plongée en profondeur:
Les fichiers temporaires ont été introduits pour la première fois dans Python 2.3 avec le module "tempfile", qui permet de créer des fichiers temporaires sur le système de fichiers. Il existe également d'autres alternatives pour stocker temporairement des données en mémoire, comme par exemple les objets StringIO. L'implémentation de la suppression automatique du fichier temporaire est assurée par le gestionnaire de contexte "with" en Python.

## Voir aussi:
- [Documentation officielle de Python pour créer des fichiers temporaires](https://docs.python.org/fr/3/library/tempfile.html)
- [Autres alternatives pour stocker des données en mémoire en Python](https://docs.python.org/fr/3/library/io.html)