---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:15.415186-07:00
description: 'Comment faire : #.'
lastmod: '2024-03-13T22:44:57.254025-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\xC9crire sur l'erreur standard"
weight: 25
---

## Comment faire :


### Utilisant `sys.stderr`
Le module intégré `sys` de Python permet une écriture explicite sur `stderr`. Cette approche est simple pour les messages d’erreur ou diagnostics basiques.

```python
import sys

sys.stderr.write('Erreur : Quelque chose a mal tourné.\n')
```
Exemple de sortie (sur stderr) :
```
Erreur : Quelque chose a mal tourné.
```

### Utilisant la fonction `print`
La fonction `print` de Python peut rediriger sa sortie vers `stderr` en spécifiant le paramètre `file`. Cette méthode est utile pour tirer parti de la convivialité de `print` lors de la gestion des messages d'erreur.
```python
from sys import stderr

print('Erreur : Défaillance dans le module.', file=stderr)
```
Exemple de sortie (sur stderr) :
```
Erreur : Défaillance dans le module.
```

### Utilisant le module `logging`
Pour une solution plus complète, le module `logging` de Python peut diriger les messages vers `stderr` et bien plus, tel que l'écriture dans un fichier ou la personnalisation du format des messages. Cette méthode est la meilleure pour les applications nécessitant différents niveaux de journalisation, de formatage des messages ou de destinations.
```python
import logging

logging.basicConfig(level=logging.WARNING)
logger = logging.getLogger(__name__)

logger.error('Erreur : La connexion à la base de données a échoué.')
```
Exemple de sortie (sur stderr) :
```
ERROR:__main__:Erreur : La connexion à la base de données a échoué.
```

### Bibliothèques tierces : `loguru`
`loguru` est une bibliothèque tierce populaire qui simplifie la journalisation dans les applications Python. Elle dirige automatiquement les erreurs vers `stderr`, parmi d'autres fonctionnalités.

Pour utiliser `loguru`, installez-le d'abord via pip :
```shell
pip install loguru
```

Ensuite, incorporez-le dans votre script Python comme suit :
```python
from loguru import logger

logger.error('Erreur : Impossible d'ouvrir le fichier.')
```
Exemple de sortie (sur stderr) :
```
2023-04-05 12:00:00.000 | ERROR    | __main__:<module>:6 - Erreur : Impossible d'ouvrir le fichier.
```
