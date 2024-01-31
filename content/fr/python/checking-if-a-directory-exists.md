---
title:                "Vérifier si un répertoire existe"
date:                  2024-01-20T14:57:57.276141-07:00
html_title:           "Go: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Vérifier l'existence d'un dossier, c'est s'assurer qu'il est là avant de tenter d'y accéder ou de le modifier. Les programmeurs font cela pour éviter les erreurs et s'assurer que leur code est robuste.

## How to (Comment faire) :
```Python
import os

# Chemin du dossier à vérifier
chemin_dossier = "mon_dossier"

# Vérifier si le chemin existe et est un dossier
if os.path.isdir(chemin_dossier):
    print(f"Le dossier '{chemin_dossier}' existe.")
else:
    print(f"Le dossier '{chemin_dossier}' n'existe pas.")

# Pour Python 3.5 et plus, on utilise pathlib
from pathlib import Path

# Chemin du dossier à vérifier avec pathlib
dossier = Path("mon_dossier")

# Vérifier si le dossier existe avec pathlib
if dossier.is_dir():
    print(f"Le dossier '{dossier}' existe.")
else:
    print(f"Le dossier '{dossier}' n'existe pas.")
```

Output (Sortie) :
```
Le dossier 'mon_dossier' n'existe pas.
Le dossier 'mon_dossier' n'existe pas.
```

## Deep Dive (Plongée Profonde) :
Historiquement, on utilisait le module `os` et `os.path` pour travailler avec les chemins de fichiers en Python. Avec Python 3.5, le module `pathlib` a été introduit pour une interface orientée objet pour les chemins de fichiers, rendant le code plus lisible.

Il y a plusieurs manières de vérifier l'existence d'un dossier :
- `os.path.exists(chemin)`: Vérifie si le chemin existe, mais ne distingue pas entre fichier et dossier.
- `os.path.isdir(chemin)`: Spécifique pour les dossiers.
- `pathlib.Path(chemin).is_dir()`: Plus moderne, utilise `pathlib`.

Les détails d'implémentation comprennent la gestion des permissions et des liens symboliques (ou "symlinks"). Il est important de savoir qu'un dossier peut exister mais ne pas être accessible en raison de restrictions de permissions.

## See Also (Voir Aussi) :
- Documentation Python sur `os.path`: https://docs.python.org/3/library/os.path.html
- Documentation Python sur `pathlib`: https://docs.python.org/3/library/pathlib.html
- Un tutoriel sur le module `os`: https://realpython.com/working-with-files-in-python/#checking-whether-a-file-exists
- Un tutoriel sur le module `pathlib`: https://realpython.com/python-pathlib/
