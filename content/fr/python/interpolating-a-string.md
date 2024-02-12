---
title:                "Interpolation d'une chaîne de caractères"
aliases:
- fr/python/interpolating-a-string.md
date:                  2024-01-28T21:23:54.078755-07:00
model:                 gpt-4-0125-preview
simple_title:         "Interpolation d'une chaîne de caractères"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/interpolating-a-string.md"
changelog:
  - 2024-01-28, dogweather, reviewed
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
L'interpolation de chaînes est la méthode permettant d'insérer des expressions au sein de littéraux de chaîne de caractères. Les programmeurs l'utilisent pour insérer dynamiquement des valeurs dans des chaînes, ce qui rend le code plus lisible et plus propre que la concaténation traditionnelle de chaînes.

## Comment faire :
Dans Python 3.6 et versions ultérieures, vous pouvez interpoler des chaînes en utilisant les f-strings. Voici comment :

```Python
name = 'Alice'
age = 30
greeting = f"Bonjour, {name}. Vous avez {age} ans."

print(greeting)
```

Sortie :
```
Bonjour, Alice. Vous avez 30 ans.
```

Vous pouvez également utiliser des expressions à l'intérieur des accolades :

```Python
a = 5
b = 10
info = f"Cinq plus dix égale {a + b}, pas {2 * (a + b)}."

print(info)
```

Sortie :
```
Cinq plus dix égale 15, pas 30.
```

## Approfondissement
Avant Python 3.6, `.format()` était la méthode privilégiée pour l'interpolation de chaînes :

```Python
name = 'Bob'
age = 25
greeting = "Bonjour, {}. Vous avez {} ans.".format(name, age)

print(greeting)
```

Le vieux Python (versions < 2.6) utilisait l'opérateur `%` pour l'interpolation, qui est moins intuitif et peut devenir compliqué avec plusieurs variables :

```Python
name = 'Carol'
age = 35
greeting = "Bonjour, %s. Vous avez %d ans." % (name, age)

print(greeting)
```

En plus d'une syntaxe plus propre, les f-strings sont plus rapides car ils sont évalués à l'exécution puis convertis directement en une opération de formatage de chaîne efficace. Les méthodes `.format()` et l'opérateur `%` impliquent plus d'étapes et sont plus lentes.

## Voir également
- [PEP 498 – Interpolation de Chaîne de Caractères Littéraux](https://www.python.org/dev/peps/pep-0498/) pour la documentation officielle sur les f-strings.
- [Les f-strings Python](https://realpython.com/python-f-strings/) par Real Python pour un tutoriel sur l'utilisation des f-strings.
- [La méthode .format()](https://docs.python.org/3/library/stdtypes.html#str.format) dans la documentation Python pour comprendre la méthode de formatage de chaînes `.format()` plus ancienne.
