---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:11.560208-07:00
description: "Comment faire : Python fournit des moyens natifs de v\xE9rifier l'existence\
  \ d'un r\xE9pertoire en utilisant les modules `os` et `pathlib`. Voici des exemples\u2026"
lastmod: '2024-03-13T22:44:57.251709-06:00'
model: gpt-4-0125-preview
summary: "Python fournit des moyens natifs de v\xE9rifier l'existence d'un r\xE9pertoire\
  \ en utilisant les modules `os` et `pathlib`."
title: "V\xE9rifier si un r\xE9pertoire existe"
weight: 20
---

## Comment faire :
Python fournit des moyens natifs de vérifier l'existence d'un répertoire en utilisant les modules `os` et `pathlib`. Voici des exemples pour les deux :

### Utiliser le module `os`
```python
import os

# Spécifier le chemin du répertoire
dir_path = "/chemin/vers/repertoire"

# Vérifier si le répertoire existe
if os.path.isdir(dir_path):
    print(f"Le répertoire {dir_path} existe.")
else:
    print(f"Le répertoire {dir_path} n'existe pas.")
```

### Utiliser le module `pathlib`
```python
from pathlib import Path

# Spécifier le chemin du répertoire
dir_path = Path("/chemin/vers/repertoire")

# Vérifier si le répertoire existe
if dir_path.is_dir():
    print(f"Le répertoire {dir_path} existe.")
else:
    print(f"Le répertoire {dir_path} n'existe pas.")
```

### Bibliothèques tierces
Bien que la bibliothèque standard de Python soit suffisante pour vérifier si un répertoire existe, des bibliothèques comme `pathlib2` peuvent être des alternatives pour une cohérence à travers les versions de Python ou pour des fonctionnalités additionnelles.

***Note :*** Avec les dernières versions de Python, `pathlib` est assez robuste pour la plupart des cas d'usage, rendant les bibliothèques tierces moins nécessaires pour cette tâche spécifique.
