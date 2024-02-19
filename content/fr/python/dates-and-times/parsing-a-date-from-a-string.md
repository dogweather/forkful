---
aliases:
- /fr/python/parsing-a-date-from-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:55.456151-07:00
description: "Analyser une date \xE0 partir d'une cha\xEEne de caract\xE8res consiste\
  \ \xE0 convertir des informations textuelles sur la date et l'heure en un objet\
  \ datetime ou un\u2026"
lastmod: 2024-02-18 23:09:08.350515
model: gpt-4-0125-preview
summary: "Analyser une date \xE0 partir d'une cha\xEEne de caract\xE8res consiste\
  \ \xE0 convertir des informations textuelles sur la date et l'heure en un objet\
  \ datetime ou un\u2026"
title: "Analyser une date depuis une cha\xEEne de caract\xE8res"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Analyser une date à partir d'une chaîne de caractères consiste à convertir des informations textuelles sur la date et l'heure en un objet datetime ou un format structuré équivalent. Cette opération est couramment effectuée pour permettre des opérations arithmétiques sur les dates, des comparaisons et des mises en forme d'une manière qui soit indépendante de la langue et de la région. Les programmeurs le font pour manipuler efficacement et gérer les données temporelles extraites de journaux, d'entrées utilisateur ou de sources externes.

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
