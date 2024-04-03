---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:47.233156-07:00
description: "Comment faire : Utiliser les regex en Python n\xE9cessite le module\
  \ `re`, qui fournit un ensemble de fonctions pour traiter le texte en utilisant\
  \ des\u2026"
lastmod: '2024-03-13T22:44:57.224465-06:00'
model: gpt-4-0125-preview
summary: "Utiliser les regex en Python n\xE9cessite le module `re`, qui fournit un\
  \ ensemble de fonctions pour traiter le texte en utilisant des expressions r\xE9\
  guli\xE8res."
title: "Utilisation des expressions r\xE9guli\xE8res"
weight: 11
---

## Comment faire :
Utiliser les regex en Python nécessite le module `re`, qui fournit un ensemble de fonctions pour traiter le texte en utilisant des expressions régulières.

### Correspondance de motifs de base
Pour rechercher un motif dans une chaîne, utilisez `re.search()`. Cela renvoie un objet de correspondance lorsque le motif est trouvé, sinon `None`.
```python
import re

text = "Learn Python programming"
match = re.search("Python", text)
if match:
    print("Motif trouvé !")
else:
    print("Motif non trouvé.")
```
Sortie :
```
Motif trouvé !
```

### Compilation des expressions régulières
Pour une utilisation répétée du même motif, compilez-le d'abord avec `re.compile()` pour de meilleures performances.
```python
pattern = re.compile("Python")
match = pattern.search("Learn Python programming")
if match:
    print("Motif compilé trouvé !")
```
Sortie :
```
Motif compilé trouvé !
```

### Division des chaînes de caractères
Pour diviser une chaîne à chaque correspondance d'un motif regex, utilisez `re.split()`.
```python
result = re.split("\s", "Python is fun")
print(result)
```
Sortie :
```
['Python', 'is', 'fun']
```

### Trouver toutes les correspondances
Pour trouver toutes les occurrences non superposées d'un motif, utilisez `re.findall()`.
```python
matches = re.findall("n", "Python programming")
print(matches)
```
Sortie :
```
['n', 'n']
```

### Remplacement de texte
Utilisez `re.sub()` pour remplacer les occurrences d'un motif par une nouvelle chaîne.
```python
replaced_text = re.sub("fun", "awesome", "Python is fun")
print(replaced_text)
```
Sortie :
```
Python is awesome
```

### Bibliothèques tierces
Bien que le module `re` intégré de Python soit puissant, les bibliothèques tierces comme `regex` offrent plus de fonctionnalités et une meilleure performance. Pour utiliser `regex`, installez-le via pip (`pip install regex`) et importez-le dans votre code.

```python
import regex

text = "Learning Python 3.8"
match = regex.search(r"Python\s(\d+\.\d+)", text)
if match:
    print(f"Version trouvée : {match.group(1)}")
```
Sortie :
```
Version trouvée : 3.8
```
