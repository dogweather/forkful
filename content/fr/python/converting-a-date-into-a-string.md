---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Gleam: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi et Quoi?

La conversion d'une date en chaîne de caractères est une méthode qui permet de transformer un objet de date Python en une chaîne de caractères. Les programmeurs le font souvent pour faciliter la présentation et le stockage des dates.

## Comment faire:

```Python
# Importer le module datetime
from datetime import datetime

# Créer une date
d = datetime(2021, 7, 2)

# Convertir la date en chaîne de caractères
str_d = d.strftime('%d-%m-%Y')

print(str_d)
```
Quand vous exécutez ce code, il produira:
```
'02-07-2021'
```

## Approfondissement

La méthode strftime est présente dans Python depuis sa première version, ce qui en fait une solution éprouvée pour convertir des dates en chaînes de caractères. Cependant, vous pouvez également utiliser la fonction format() pour obtenir le même résultat. Les détails de mise en œuvre de cette conversion sont relativement simples, des méthodes intégrées à Python étant disponibles pour cette tâche.

```Python
# Exemple en utilisant la fonction format()
str_d = "{:%d-%m-%Y}".format(d)

print(str_d)
```

Cela donne le même résultat:
```
'02-07-2021'
```

## Voir aussi

Pour plus d'informations sur la manipulation des dates en Python, consultez les ressources suivantes:

1. Documentation officielle de Python sur le module datetime: https://docs.python.org/fr/3/library/datetime.html
2. Tutoriel sur le site Real Python: https://realpython.com/python-datetime/
3. Post de blog technique sur le formatage des dates en Python: https://www.geeksforgeeks.org/python-format-function/