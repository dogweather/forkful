---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:55.456151-07:00
description: "Comment faire : La biblioth\xE8que standard de Python fournit le module\
  \ `datetime`, qui inclut la m\xE9thode `strptime` \xE0 cet effet. La m\xE9thode\
  \ n\xE9cessite deux\u2026"
lastmod: '2024-03-13T22:44:57.246601-06:00'
model: gpt-4-0125-preview
summary: "La biblioth\xE8que standard de Python fournit le module `datetime`, qui\
  \ inclut la m\xE9thode `strptime` \xE0 cet effet."
title: "Analyser une date depuis une cha\xEEne de caract\xE8res"
weight: 30
---

## Comment faire :
La bibliothèque standard de Python fournit le module `datetime`, qui inclut la méthode `strptime` à cet effet. La méthode nécessite deux arguments : la chaîne de caractères de la date et une directive de format qui spécifie le motif de la chaîne d'entrée.

```python
from datetime import datetime

# Exemple de chaîne
date_string = "2023-04-01 14:30:00"
# Analyse de la chaîne en objet datetime
parsed_date = datetime.strptime(date_string, "%Y-%m-%d %H:%M:%S")

print(parsed_date)
# Sortie : 2023-04-01 14:30:00
```

Pour une analyse de date plus nuancée, en particulier lorsqu'on traite de plusieurs formats ou localités, la bibliothèque tierce `dateutil` peut être extrêmement utile. Elle fournit un module parseur qui peut analyser les dates dans presque tous les formats de chaîne.

```python
from dateutil import parser

# Exemples de chaînes
date_string1 = "April 1, 2023 2:30 PM"
date_string2 = "1st April 2023 14:30"

# Utilisation du parseur de dateutil
parsed_date1 = parser.parse(date_string1)
parsed_date2 = parser.parse(date_string2)

print(parsed_date1)
# Sortie : 2023-04-01 14:30:00
print(parsed_date2)
# Sortie : 2023-04-01 14:30:00
```

`dateutil` est compétent pour gérer la plupart des formats de date sans chaînes de format explicites, ce qui en fait un choix polyvalent pour les applications traitant de diverses représentations de dates.
