---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:05.907354-07:00
description: "Mettre une cha\xEEne en capitales signifie convertir le premier caract\xE8\
  re d'une cha\xEEne en majuscule et le reste en minuscule. Cette op\xE9ration est\
  \ couramment\u2026"
lastmod: '2024-03-13T22:44:57.216520-06:00'
model: gpt-4-0125-preview
summary: "Mettre une cha\xEEne en capitales signifie convertir le premier caract\xE8\
  re d'une cha\xEEne en majuscule et le reste en minuscule."
title: "Mettre en majuscule une cha\xEEne"
weight: 2
---

## Comment faire :


### En utilisant la méthode intégrée de Python :
Python dispose d'une méthode intégrée `.capitalize()` pour les chaînes permettant de réaliser cette tâche facilement.

```python
my_string = "hello world"
capitalized_string = my_string.capitalize()
print(capitalized_string)
```
**Sortie :**
```
Hello world
```

### Gérer plusieurs mots :
Pour les scénarios où vous souhaitez que chaque mot dans une chaîne commence par une lettre majuscule (comme pour les titres), la méthode `.title()` peut être appliquée.

```python
my_title = "python programming essentials"
title_case = my_title.title()
print(title_case)
```
**Sortie :**
```
Python Programming Essentials
```

### Utiliser des bibliothèques tierces :
Bien que la bibliothèque standard de Python soit équipée pour la mise en capitale de chaînes basique, des bibliothèques comme `textblob` peuvent offrir un contrôle plus nuancé, en particulier pour le traitement du langage naturel.

D'abord, assurez-vous d'avoir `textblob` installé :
```bash
pip install textblob
```

Ensuite, utilisez-le pour mettre des chaînes en capitales, en gardant à l'esprit que le capitalize de `textblob` peut fonctionner différemment selon le contexte d'utilisation :

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

Souvenez-vous, tandis que les méthodes `capitalize()` et `title()` sont universellement utiles, exploiter des bibliothèques comme `textblob` peut fournir une flexibilité supplémentaire pour des applications spécifiques.
