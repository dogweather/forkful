---
title:                "Obtenir la date actuelle"
date:                  2024-01-20T15:15:54.584159-07:00
html_title:           "C: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
On récupère la date actuelle pour des logs, des timestamps, des features temporelles. C'est basique, mais on fait ça tout le temps.

## How to: (Comment faire :)
Utilisons `datetime` pour avoir la date aujourd'hui. Simple et efficace.

```Python
from datetime import date

# Obtenir la date d'aujourd'hui
aujourdhui = date.today()

# Afficher la date
print(aujourdhui)
```

Résultat :
```
2023-04-04
```

## Deep Dive (Plongée en Profondeur)
`datetime` existe depuis Python 2.3. Avant, on utilisait le module `time`, mais `datetime` est plus high-level. Il y a aussi des librairies tierces comme `arrow` ou `pendulum` pour plus de fonctionnalités. En interne, `datetime` représente les dates avec des objets, ce qui les rend manipulables.

## See Also (Voir Aussi)
- [Documentation officielle datetime](https://docs.python.org/3/library/datetime.html)
- [PyPI arrow](https://pypi.org/project/arrow/)
- [PyPI pendulum](https://pypi.org/project/pendulum/)
