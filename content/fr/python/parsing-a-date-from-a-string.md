---
title:                "Analyser une date à partir d'une chaîne"
html_title:           "Clojure: Analyser une date à partir d'une chaîne"
simple_title:         "Analyser une date à partir d'une chaîne"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

L'analyse d'une date à partir d'une chaîne de caractères consiste à convertir un texte formaté en objet date reconnaissable par le programme. Les programmeurs le font pour manipuler et utiliser les dates en tant qu'objet au lieu de simples chaînes.

## Comment faire:

Voici comment vous pouvez convertir une chaîne en date en utilisant la bibliothèque `datetime` de Python.

```Python
from datetime import datetime

date_string = "20/04/2020 20:20:20"

date_object = datetime.strptime(date_string, "%d/%m/%Y %H:%M:%S")

print(date_object)
```

Exécutez ce code, vous obtiendrez :

```Python
2020-04-20 20:20:20
```

## Plongée en profondeur

Historiquement, les dates étaient principalement manipulées comme des chaînes de caractères. Cependant, la manipulation de dates en tant qu'objets offre une plus grande flexibilité et contrôle, d'où le besoin d'analyser les chaînes de date.

Alternativement, on pourrait utiliser la bibliothèque `dateutil` qui peut analyser les dates d'une manière plus flexible et complète sans avoir à préciser le format de la date.

Il est important de noter que `datetime.strptime()` est une méthode qui prend deux arguments : la chaîne de date à analyser et le format de la date.

## Voir Aussi

- Documentation Python sur la bibliothèque datetime : https://docs.python.org/3/library/datetime.html
- Documentation Python sur la bibliothèque dateutil : https://dateutil.readthedocs.io/en/stable/
- Pour plus de détails sur le formatage de la date : https://strftime.org/