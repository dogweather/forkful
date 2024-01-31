---
title:                "Écrire dans l'erreur standard"
date:                  2024-01-19
html_title:           "Arduino: Écrire dans l'erreur standard"
simple_title:         "Écrire dans l'erreur standard"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Ecrire dans l'erreur standard (stderr) permet de séparer les messages d'erreurs des résultats normaux (stdout). Les développeurs font ça pour faciliter le débogage et la gestion des logs.

## How to:

Utilisez `sys.stderr` pour écrire des erreurs :

```Python
import sys

print("Ceci est un message normal", file=sys.stdout)  # sortie standard
print("Ceci est un message d'erreur", file=sys.stderr)  # erreur standard
```

Sortie :

```
Ceci est un message normal
Ceci est un message d'erreur
```

## Deep Dive
Traditionnellement, les systèmes Unix-like ont trois canaux de communication principaux : stdin, stdout, et stderr (0, 1, et 2, respectivement). Utiliser stderr pour les erreurs permet de redirecter ou de filtrer ces messages séparément. Il y a des alternatives comme les frameworks de logging en Python, mais stderr reste un mécanisme à bas niveau simple et puissant. Son implémentation en Python est directe, faisant partie du module `sys` de la bibliothèque standard.

## See Also
Pour plus d'informations :

- La documentation Python sur le module sys : https://docs.python.org/3/library/sys.html
- Explication sur l'I/O standard : https://en.wikipedia.org/wiki/Standard_streams
- Guide sur le logging en Python : https://docs.python.org/3/howto/logging.html
