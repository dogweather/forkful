---
title:                "Analyse d'une date à partir d'une chaîne de caractères"
date:                  2024-01-20T15:38:15.110736-07:00
html_title:           "Arduino: Analyse d'une date à partir d'une chaîne de caractères"
simple_title:         "Analyse d'une date à partir d'une chaîne de caractères"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi & Pourquoi ?)

Parse un date d'une chaîne de caractères, c'est convertir le texte en un objet date que Python comprend. Les programmeurs le font pour manipuler les dates, faire des calculs de temps, ou stocker les dates dans un format standard.

## How to: (Comment faire :)

```Python
from datetime import datetime

# Parse une date depuis une chaîne de caractères
date_str = "01/04/2023"
date_obj = datetime.strptime(date_str, "%d/%m/%Y")

print(date_obj)  # Affiche: 2023-04-01 00:00:00
```

```Python
# Formatte une date vers une chaîne de caractères
nouvelle_chaine = date_obj.strftime("%A, %d %B %Y")
print(nouvelle_chaine)  # Affiche par exemple: Saturday, 01 April 2023
```

## Deep Dive (Plongée en Profondeur)

Historiquement, les programmeurs devaient souvent parser les dates manuellement, ce qui était source d'erreurs. Python a introduit `datetime.strptime()` pour simplifier le processus. 

Alternatives:
- `dateutil.parser`: Une bibliothèque tierce qui peut parser des dates dans des formats plus variés et toléré les erreurs.
- `pandas.to_datetime`: Si vous travaillez déjà avec Pandas, c'est une méthode puissante avec beaucoup d'options de parsing.

Détails d'implémentation:
- `datetime.strptime()` nécessite la définition explicite du format de la chaîne de caractères. Si le format ne correspond pas, vous aurez une ValueError.
- L'objet date résultant peut être manipulé (ajouter ou soustraire des jours, comparer avec d'autres dates, etc.).

## See Also (Voir Aussi)

- Documentation Python sur `datetime`: https://docs.python.org/3/library/datetime.html
- `dateutil.parser` documentation: https://dateutil.readthedocs.io/en/stable/parser.html
- Pandas `to_datetime` fonction: https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.to_datetime.html