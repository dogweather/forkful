---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: "Comment faire : Je fais cela assez souvent pour avoir refactoris\xE9\
  \ cela dans cette simple fonction `delete()`. C'est aussi une bonne d\xE9monstration\
  \ des\u2026"
lastmod: '2024-04-05T21:53:58.801811-06:00'
model: gpt-4-0125-preview
summary: "Je fais cela assez souvent pour avoir refactoris\xE9 cela dans cette simple\
  \ fonction `delete()`."
title: "Suppression de caract\xE8res correspondant \xE0 un motif"
weight: 5
---

## Comment faire :
```Python
import re

# Chaîne d'exemple
texte = "Bonjour, Monde ! 1234"

# Supprimer tous les chiffres
sans_chiffres = re.sub(r'\d', '', texte)
print(sans_chiffres)  # Sortie : "Bonjour, Monde ! "

# Supprimer la ponctuation
sans_ponctuation = re.sub(r'[^\w\s]', '', texte)
print(sans_ponctuation)  # Sortie : "Bonjour Monde 1234"

# Supprimer les voyelles
sans_voyelles = re.sub(r'[aeiouAEIOU]', '', texte)
print(sans_voyelles)  # Sortie : "Bnjr, Mnd! 1234"
```

### Ma fonction personnalisée

Je fais cela assez souvent pour avoir refactorisé cela dans cette simple fonction `delete()`. C'est aussi une bonne démonstration des [doctests](https://docs.python.org/3/library/doctest.html) :

```python
def delete(chaine: str, regex: str) -> str:
    """
    >>> delete("Bonjour, le monde !", "l")
    'Bonjour, e monde !'

    >>> delete("Bonjour, le monde !", "[a-z]")
    'B,  !'
    """
    return re.sub(regex, "", chaine)
```



## Plongée profonde
La pratique de supprimer des caractères correspondant à un modèle dans le texte a des racines profondes en informatique, remontant aux outils Unix initiaux comme `sed` et `grep`. En Python, le module `re` offre cette capacité, tirant parti des expressions régulières, un outil puissant et polyvalent pour le traitement du texte.

Les alternatives au module `re` comprennent :
- Les méthodes de chaîne comme `replace()` pour les cas simples.
- Les bibliothèques tierces comme `regex` pour des motifs plus complexes et une meilleure prise en charge Unicode.

Sous le capot, lorsque vous utilisez `re.sub()`, l'interprète Python compile le motif en une série de codes d'opération, traités par une machine à états qui effectue une correspondance de motifs directement sur le texte d'entrée. Cette opération peut être gourmande en ressources pour de grandes chaînes ou des motifs complexes, donc les considérations de performance sont cruciales pour le traitement des grandes données.

## Voir aussi
- [Documentation du module `re` Python](https://docs.python.org/3/library/re.html) : Documents officiels pour les expressions régulières en Python.
- [Regular-Expressions.info](https://www.regular-expressions.info/) : Un guide complet sur les expressions régulières.
- [Tutoriel Real Python sur regex](https://realpython.com/regex-python/) : Applications réelles des expressions régulières en Python.
