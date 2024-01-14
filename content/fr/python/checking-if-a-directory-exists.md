---
title:                "Python: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Le fait de vérifier si un répertoire existe est une tâche couramment utilisée en programmation Python. Cela peut être utile pour s'assurer qu'un dossier spécifique existe avant d'effectuer des opérations de lecture ou d'écriture de fichiers à l'intérieur de celui-ci. Dans cet article, nous allons expliquer comment vérifier si un répertoire existe en utilisant du code Python.

## Comment Faire

Pour vérifier si un répertoire existe en Python, nous allons utiliser le module `os` qui fournit des méthodes pour interagir avec le système d'exploitation. La méthode `path.exists()` de ce module nous permet de vérifier si un chemin spécifique existe. Voici un exemple de code:

```Python
## Importer le module os
import os

## Spécifier le chemin du répertoire à vérifier
chemin = "/chemin/vers/mon/répertoire"

## Vérifier si le répertoire existe
if os.path.exists(chemin):
    print("Le répertoire existe!")
else:
    print("Le répertoire n'existe pas.")
```

Si le répertoire spécifié existe, le programme affichera "Le répertoire existe!". Sinon, il affichera "Le répertoire n'existe pas.".

## Plongée Profonde

Il est important de noter que la méthode `path.exists()` ne vérifie que l'existence du chemin, pas nécessairement si c'est un répertoire. Pour vérifier spécifiquement si un répertoire existe, vous pouvez utiliser la méthode `path.isdir()`. De plus, si vous souhaitez créer un répertoire s'il n'existe pas encore, vous pouvez utiliser la méthode `os.makedirs()`. Voici un exemple de code:

```Python
## Importer le module os
import os

## Spécifier le chemin du répertoire à vérifier
chemin = "/chemin/vers/mon/répertoire"

## Vérifier si le répertoire existe et en créer un s'il n'existe pas
if not os.path.isdir(chemin):
    os.makedirs(chemin)
```

Cela créera le répertoire spécifié s'il n'existe pas encore.

## Voir Aussi

Si vous souhaitez en savoir plus sur la manipulation des répertoires en Python, voici quelques liens utiles:

- Documentation officielle de Python sur le module `os`: https://docs.python.org/fr/3/library/os.html
- Tutoriel sur la manipulation des fichiers et répertoires en Python: https://www.digitalocean.com/community/tutorials/comment-manipuler-les-fichiers-et-les-repertoires-en-python-3-fr
- Guide sur les chemins de fichiers en Python: https://realpython.com/python-pathlib/

Merci d'avoir lu cet article sur la vérification de l'existence d'un répertoire en Python. Nous espérons que cela vous a été utile dans vos projets de programmation. À bientôt pour de nouveaux articles sur le langage Python!