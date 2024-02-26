---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:47.233156-07:00
description: "Les expressions r\xE9guli\xE8res (regex) sont des motifs utilis\xE9\
  s pour rechercher des combinaisons de caract\xE8res dans des cha\xEEnes de texte.\
  \ Les programmeurs les\u2026"
lastmod: '2024-02-25T18:49:54.117420-07:00'
model: gpt-4-0125-preview
summary: "Les expressions r\xE9guli\xE8res (regex) sont des motifs utilis\xE9s pour\
  \ rechercher des combinaisons de caract\xE8res dans des cha\xEEnes de texte. Les\
  \ programmeurs les\u2026"
title: "Utilisation des expressions r\xE9guli\xE8res"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Les expressions régulières (regex) sont des motifs utilisés pour rechercher des combinaisons de caractères dans des chaînes de texte. Les programmeurs les utilisent pour chercher, éditer ou manipuler du texte en fonction de motifs définis, ce qui les rend indispensables pour des tâches comme la validation de données, l'analyse syntaxique ou la transformation.

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
