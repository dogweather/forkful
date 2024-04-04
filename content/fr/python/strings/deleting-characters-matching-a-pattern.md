---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: 'Comment faire : .'
lastmod: '2024-04-04T01:27:54.819520-06:00'
model: gpt-4-0125-preview
summary: .
title: "Supprimer des caract\xE8res correspondant \xE0 un mod\xE8le"
weight: 5
---

## Comment faire :
```Python
import re

# Chaîne d'exemple
text = "Hello, World! 1234"

# Supprimer tous les chiffres
sans_chiffres = re.sub(r'\d', '', text)
print(sans_chiffres)  # Sortie : "Hello, World! "

# Supprimer la ponctuation
sans_ponctuation = re.sub(r'[^\w\s]', '', text)
print(sans_ponctuation)  # Sortie : "Hello World 1234"

# Supprimer les voyelles
sans_voyelles = re.sub(r'[aeiouAEIOU]', '', text)
print(sans_voyelles)  # Sortie : "Hll, Wrld! 1234"
```

### Une fonction personnalisée que j'ai écrite

Je fais cela assez souvent pour que j'ai refactorisé cela dans cette fonction `delete()`. C'est aussi une bonne démonstration des [doctests](https://docs.python.org/3/library/doctest.html) :

```python
def delete(string: str, regex: str) -> str:
    """
    >>> delete("Hello, world!", "l")
    'Heo, word!'

    >>> delete("Hello, world!", "[a-z]")
    'H, !'
    """
    return re.sub(regex, "", string)
```



## Approfondissement
La pratique de supprimer des caractères correspondant à un motif dans un texte a des racines profondes en informatique, remontant à des outils Unix primitifs comme `sed` et `grep`. En Python, le module `re` fournit cette capacité, en exploitant les expressions régulières, un outil puissant et polyvalent pour le traitement de texte.

Les alternatives au module `re` incluent :
- Les méthodes de chaîne comme `replace()` pour les cas simples.
- Les bibliothèques tierces comme `regex` pour des motifs plus complexes et une meilleure prise en charge d'Unicode.

Sous le capot, lorsque vous utilisez `re.sub()`, l'interpréteur Python compile le motif en une série de bytecode, traité par une machine à états qui effectue la correspondance de motifs directement sur le texte d'entrée. Cette opération peut être intensive en ressources pour des chaînes de grande taille ou des motifs complexes, donc les considérations de performance sont cruciales pour le traitement de grandes données.

## Voir aussi
- [Documentation du module `re` de Python](https://docs.python.org/3/library/re.html) : documents officiels pour les expressions régulières en Python.
- [Regular-Expressions.info](https://www.regular-expressions.info/) : Un guide complet des expressions régulières.
- [Tutoriel de Real Python sur regex](https://realpython.com/regex-python/) : Applications réelles des expressions régulières en Python.
