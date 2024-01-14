---
title:    "Python: Vérifier si un répertoire existe"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Pourquoi

Avant de plonger dans les détails, il est important de comprendre pourquoi il est utile de vérifier si un répertoire existe en Python. En général, cette vérification est importante lors de la manipulation de fichiers ou de la création de nouveaux fichiers, pour s'assurer que le répertoire cible existe avant d'y accéder. Sans cette vérification, l'exécution du code risque de provoquer des erreurs.

## Comment faire

Pour vérifier si un répertoire existe en Python, il existe deux approches principales : utiliser la méthode `exists()` du module `os` ou la méthode `isdir()` du module `path`. Voici un exemple de code pour chacune de ces méthodes :

```Python
import os
import os.path as path

# utilisation de la méthode exists()
directory = "chemin/vers/repertoire" 
if os.path.exists(directory):
    print("Le répertoire existe") 
else:
    print("Le répertoire n'existe pas")

# utilisation de la méthode isdir()
directory = "chemin/vers/repertoire"
if os.path.isdir(directory):
    print("Le répertoire existe") 
else:
    print("Le répertoire n'existe pas")
```

La première approche utilise la méthode `exists()` qui renvoie `True` si le fichier ou le répertoire existe, et `False` dans le cas contraire. La seconde approche utilise la méthode `isdir()` qui renvoie également `True` si l'objet passé en paramètre est un répertoire, et `False` s'il s'agit d'un fichier ou s'il n'existe pas.

## Plongeon en profondeur

Si vous souhaitez aller plus loin dans la vérification de l'existence d'un répertoire, il est important de comprendre quels sont les types d'erreurs qui peuvent se produire et comment les gérer. Par exemple, si le répertoire cible est situé sur un lecteur réseau, il est possible qu'il y ait des problèmes de permission d'accès. Dans ce cas, vous pouvez utiliser la méthode `access()` du module `os` pour vérifier les permissions avant d'accéder au répertoire.

## Voir aussi

- [Documentation officielle de Python pour la méthode `exists()`](https://docs.python.org/fr/3/library/os.path.html#os.path.exists)
- [Documentation officielle de Python pour la méthode `isdir()`](https://docs.python.org/fr/3/library/os.path.html#os.path.isdir)
- [Documentation officielle de Python pour la méthode `access()`](https://docs.python.org/fr/3/library/os.html#os.access)