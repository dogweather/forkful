---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:34.962078-07:00
description: "Comment faire : Python dispose d'une m\xE9thode int\xE9gr\xE9e `.capitalize()`\
  \ pour les cha\xEEnes de caract\xE8res permettant de r\xE9aliser cette t\xE2che\
  \ facilement."
lastmod: '2024-04-04T00:26:51.803717-06:00'
model: gpt-4-0125-preview
summary: ''
title: "Mettre une cha\xEEne en majuscules"
weight: 2
---

## Comment faire :

### Utilisation de la méthode intégrée de Python :
Python dispose d'une méthode intégrée `.capitalize()` pour les chaînes de caractères permettant de réaliser cette tâche facilement.

```python
my_string = "hello world"
capitalized_string = my_string.capitalize()
print(capitalized_string)
```
**Sortie :**
```
Hello world
```

Voici ma propre méthode `capitalize()` personnalisée que j'utilise pour construire ce site. Il fallait que je m'assure que des mots spéciaux comme **HTML** restent toujours en majuscules. Cela démontre également l'utilisation de [doctests](https://docs.python.org/3/library/doctest.html) :

```python
def capitalize(string: str) -> str:
    """
    Mettre en majuscule une chaîne, c'est-à-dire rendre la première lettre en majuscule.
    Gérer des cas spéciaux comme "HTML".

    >>> capitalize("this is html, csv, xml, and http (no REPL).")
    'This is HTML, CSV, XML, and HTTP (no REPL).'

    >>> capitalize("this is json, VBA, an IDE, and yaml in the CLI.")
    'This is JSON, VBA, an IDE, and YAML in the CLI.'
    """
    return (
        string
            .capitalize()
            .replace('cli',  'CLI')
            .replace('csv',  'CSV')
            .replace('html', 'HTML')
            .replace('http', 'HTTP')
            .replace('ide',  'IDE')
            .replace('json', 'JSON')
            .replace('repl', 'REPL')
            .replace('vba',  'VBA')
            .replace('xml',  'XML')
            .replace('yaml', 'YAML')
    )

```

### Gérer plusieurs mots :
Pour les scénarios où vous souhaitez que chaque mot dans une chaîne commence par une lettre majuscule (comme les titres), la méthode `.title()` peut être appliquée.

```python
my_title = "python programming essentials"
title_case = my_title.title()
print(title_case)
```
**Sortie :**
```
Python Programming Essentials
```

### Utilisation de bibliothèques tierces :
Alors que la bibliothèque standard de Python est équipée pour la capitalisation de base des chaînes, des bibliothèques telles que `textblob` peuvent offrir un contrôle plus nuancé, en particulier pour le traitement du langage naturel.

Tout d'abord, assurez-vous d'avoir `textblob` installé :
```bash
pip install textblob
```

Ensuite, utilisez-le pour mettre en majuscule des chaînes, en gardant à l'esprit que la capitalisation de `textblob` peut fonctionner différemment en fonction du contexte d'utilisation :

```python
from textblob import TextBlob

my_sentence = "this is a test sentence"
blob = TextBlob(my_sentence)
capitalized_blob = TextBlob(blob.string.capitalize())
print(capitalized_blob)
```
**Sortie :**
```
This is a test sentence
```

Rappelez-vous, bien que les méthodes `capitalize()` et `title()` soient universellement utiles, l'exploitation de bibliothèques comme `textblob` peut offrir une flexibilité supplémentaire pour des applications spécifiques.
