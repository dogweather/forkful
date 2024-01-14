---
title:    "Python: Vérification de l'existence d'un répertoire"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Saviez-vous qu'il est possible de vérifier si un dossier existe en utilisant Python ? Cela peut être utile pour s'assurer qu'un certain dossier est bien présent avant d'exécuter le reste du code.

## Comment faire

Pour vérifier si un dossier existe, nous pouvons utiliser le module `os` de Python et plus précisément la fonction `path.exists()`. Voici un exemple de code :

```Python
import os

if os.path.exists("/chemin/vers/mon/dossier"):
  print("Le dossier existe !")
else:
  print("Le dossier n'existe pas.")
```

Voici un exemple de sortie si le dossier existait :

```
Le dossier existe !
```

Vous pouvez également utiliser le module `os.path` pour vérifier si un fichier existe en utilisant la fonction `isfile()` ou si un lien symbolique existe en utilisant la fonction `islink()`.

## Un peu plus en profondeur

En utilisant la fonction `path.exists()`, il est important de noter qu'elle vérifie à la fois les dossiers et les fichiers. Si vous voulez uniquement vérifier si un dossier existe, vous pouvez utiliser la fonction `path.isdir()`.

De plus, il est important de prendre en compte les permissions de fichier lors de la vérification si le dossier existe. Si vous n'avez pas les permissions nécessaires, la fonction peut renvoyer une erreur même si le dossier existe réellement.

## Voir aussi

- La documentation officielle de Python sur le module `os` : https://docs.python.org/fr/3/library/os.html
- Un tutoriel sur la vérification des dossiers et fichiers en Python : https://www.digitalocean.com/community/tutorials/how-to-use-the-os-module-in-python-3
- Une discussion sur Stack Overflow à propos des permissions lors de la vérification des dossiers : https://stackoverflow.com/questions/21185127/permission-denied-error-when-using-os-path-exists-in-python