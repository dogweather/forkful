---
title:                "Utilisation des expressions régulières"
date:                  2024-01-19
html_title:           "Bash: Utilisation des expressions régulières"
simple_title:         "Utilisation des expressions régulières"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
("## Quoi et Pourquoi ?")
Les expressions régulières (regex) permettent de chercher, filtrer et manipuler du texte. Les programmeurs les utilisent pour automatiser et simplifier le traitement des chaînes de caractères.

## How to:
("## Comment faire :")
Exemple de base pour chercher un motif dans une chaîne de caractères :

```Python
import re

texte = "Trouvons les dates: 2023-04-12, 2024-05-19."
pattern = r"\d{4}-\d{2}-\d{2}"

dates = re.findall(pattern, texte)
print(dates)
```

Sortie :
```
['2023-04-12', '2024-05-19']
```

## Deep Dive
("## Plongée en Profondeur")
Les expressions régulières sont nées dans les années 1950 avec les travaux de Stephen Kleene. Python propose le module `re` pour les regex, mais il existe aussi d'autres modules comme `regex`. Concernant l’implémentation, les regex en Python sont basées sur des automates et peuvent varier en termes d'efficacité.

## See Also
("## Voir Aussi")
- Documentation Python officielle sur regex : https://docs.python.org/3/library/re.html
- Tutoriel Regex interactif : https://regexone.com/
- Article détaillé sur l'efficacité des regex : https://www.regular-expressions.info/optimization.html
