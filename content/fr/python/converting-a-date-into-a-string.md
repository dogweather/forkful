---
title:                "Conversion d'une date en chaîne de caractères"
date:                  2024-01-20T17:37:07.614961-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversion d'une date en chaîne de caractères"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Convertir une date en chaîne de caractères permet de l'afficher selon un format lisible pour un humain. Les programmeurs font ça pour enregistrer ou communiquer des informations de date de façon standardisée ou pour répondre à des besoins spécifiques d’internationalisation.

## How to: (Comment Faire ?)
```Python
from datetime import datetime

# Créer un objet date
date_actuelle = datetime.now()

# Convertir en chaîne sous le format "jour mois année"
date_en_chaine = date_actuelle.strftime("%d %B %Y")
print(date_en_chaine)  # Par exemple "05 April 2023"

# Convertir en chaîne avec format standard ISO 8601
date_iso8601 = date_actuelle.isoformat()
print(date_iso8601)  # Par exemple "2023-04-05T14:45:30.000123"
```

## Deep Dive (Plongée en Profondeur)
Historiquement, les formats de date sont chaotiques : différents pays utilisent différentes conventions. En Python, `strftime` et `strptime` sont les méthodes traditionnellement utilisées pour convertir entre objets `datetime` et chaînes de caractères.
`isoformat` est une méthode plus récente qui renvoie une date au format ISO 8601 - le standard international pour la représentation des dates et des heures.
Au niveau de l'implémentation, Python utilise des directives de format spécifiques (comme `%Y` pour l'année complète) que vous pouvez mixer pour personnaliser le résultat de `strftime`.

## See Also (Voir Aussi)
- La documentation Python `datetime` : https://docs.python.org/fr/3/library/datetime.html
- La documentation Python pour `strftime` et `strptime` : https://docs.python.org/fr/3/library/datetime.html#strftime-and-strptime-behavior
- Wikipédia sur le format ISO 8601 : https://fr.wikipedia.org/wiki/ISO_8601