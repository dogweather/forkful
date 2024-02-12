---
title:                "Obtenir la date actuelle"
aliases:
- /fr/python/getting-the-current-date/
date:                  2024-02-03T19:10:28.434067-07:00
model:                 gpt-4-0125-preview
simple_title:         "Obtenir la date actuelle"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

L'obtention de la date actuelle en Python est une opération essentielle pour de nombreuses applications, telles que la journalisation, l'analyse de données et la prise de décisions basée sur le temps. Il s'agit de récupérer la date actuelle du système, ce qui est crucial pour les tâches dépendant du contexte temporel.

## Comment faire :

**En utilisant la bibliothèque standard `datetime` :**

Le module `datetime` dans la bibliothèque standard de Python fournit des classes pour manipuler des dates et des heures. Pour obtenir la date actuelle, vous pouvez utiliser la méthode `date.today()`.

```python
from datetime import date

aujourd'hui = date.today()
print(aujourd'hui)  # Sortie : AAAA-MM-JJ (par exemple, 2023-04-05)
```

**Formatage du temps :**

Si vous avez besoin de la date actuelle dans un format différent, la méthode `strftime` vous permet de spécifier un formatage de date personnalisé :

```python
from datetime import date

aujourd'hui = date.today()
date_formatee = aujourd'hui.strftime('%B %d, %Y')  # Format d'exemple : "Avril 05, 2023"
print(date_formatee)
```

**Utiliser `pendulum` pour plus de flexibilité (une bibliothèque tierce populaire) :**

`Pendulum` est une bibliothèque tierce qui offre une approche plus intuitive pour gérer les dates et les heures en Python. Elle étend les fonctionnalités de datetime standard et simplifie la gestion des fuseaux horaires, entre autres fonctionnalités.

Tout d'abord, assurez-vous d'avoir installé `pendulum` via pip :

```shell
pip install pendulum
```

Ensuite, pour obtenir la date actuelle :

```python
import pendulum

aujourd'hui = pendulum.now().date()
print(aujourd'hui)  # Sortie : AAAA-MM-JJ (par exemple, 2023-04-05)
```

Avec `pendulum`, le formatage est également simple et similaire à l'approche `strftime` :

```python
import pendulum

aujourd'hui = pendulum.now()
date_formatee = aujourd'hui.to_formatted_date_string()  # Format par défaut : "Apr 5, 2023"
print(date_formatee)
```
