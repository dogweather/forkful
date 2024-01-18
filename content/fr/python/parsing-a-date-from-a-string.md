---
title:                "Analyser une date à partir d'une chaîne de caractères."
html_title:           "Python: Analyser une date à partir d'une chaîne de caractères."
simple_title:         "Analyser une date à partir d'une chaîne de caractères."
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

Le parsing de date à partir d'une chaîne de caractères est un processus qui consiste à convertir une date représentée sous forme de texte en un format utilisable par un programme informatique. Les programmeurs font cela pour pouvoir manipuler et utiliser des dates dans leurs applications, comme la gestion d'événements ou la création de rapports en fonction de dates spécifiques.

## Comment faire:

Voici un exemple de code en Python pour parser une date à partir d'une chaîne de caractères:

```Python
from datetime import datetime

date_string = "10/12/2021"
parsed_date = datetime.strptime(date_string, '%d/%m/%Y').date()

print(parsed_date)
```

Résultat:
```Python
2021-12-10
```

La méthode ```strptime()``` prend deux arguments: la chaîne de caractères contenant la date et le format de la date spécifiée en utilisant des symboles spéciaux. Dans cet exemple, le symbole ```%d``` représente le jour, ```%m``` le mois et ```%Y``` l'année. La méthode ```date()``` permet ensuite d'extraire uniquement la date sans l'heure.

## Plongée en profondeur:

Ce processus de parsing de date est basé sur les formats de date standardisés tels que le format ISO 8601 (YYYY-MM-DD) et peut varier en fonction de la langue ou de la région de l'utilisateur. Avant l'introduction de la méthode ```strptime()``` en Python 2.2, les programmeurs devaient utiliser des fonctions plus complexes ou des bibliothèques tierces comme ```time``` et ```calendar``` pour convertir les dates en objets utilisables.

Il existe également d'autres moyens de parser des dates à partir de chaînes de caractères, tels que la bibliothèque ```dateutil``` qui offre une plus grande flexibilité dans la reconnaissance de formats de date différents.

## Voir aussi:

- [Documentation Python sur la méthode strptime()](https://docs.python.org/fr/3/library/datetime.html#strftime-strptime-behavior)
- [Bibliothèque dateutil pour une analyse avancée des dates](https://pypi.org/project/python-dateutil/) 
- [Norme ISO 8601 pour les formats de date et d'heure](https://fr.wikipedia.org/wiki/ISO_8601)