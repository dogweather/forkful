---
date: 2024-01-20 17:35:27.524482-07:00
description: "Concat\xE9ner des cha\xEEnes de caract\xE8res, c'est les coller bout\
  \ \xE0 bout pour en faire une seule. Les programmeurs font \xE7a pour afficher des\
  \ messages\u2026"
lastmod: '2024-03-13T22:44:57.226409-06:00'
model: gpt-4-1106-preview
summary: "Concat\xE9ner des cha\xEEnes de caract\xE8res, c'est les coller bout \xE0\
  \ bout pour en faire une seule."
title: "Concat\xE9nation de cha\xEEnes de caract\xE8res"
weight: 3
---

## Quoi et Pourquoi ?
Concaténer des chaînes de caractères, c'est les coller bout à bout pour en faire une seule. Les programmeurs font ça pour afficher des messages personnalisés, composer des URL, ou gérer du texte dynamique.

## Comment faire :
```Python
# Concaténation simple avec le signe +
salutation = "Bonjour"
nom = "Alex"
message = salutation + ", " + nom + "!"
print(message)  # Bonjour, Alex!

# Avec la méthode join()
couleurs = ["rouge", "vert", "bleu"]
palette = ", ".join(couleurs)
print("Couleurs disponibles: " + palette)  # Couleurs disponibles: rouge, vert, bleu

# En utilisant les f-strings (Python 3.6+)
age = 25
phrase = f"Tu as {age} ans."
print(phrase)  # Tu as 25 ans.
```

## Plongée Profonde
Historiquement, la concaténation de chaînes en Python pouvait être plus coûteuse car chaque opération créait un nouvel objet chaîne. La méthode `join()` et les "string literals" comme les f-strings sont plus efficaces. Ces méthodes permettent également une meilleure lisibilité et facilitent le formatage.

Alternativement, la concaténation peut se faire avec des outils comme `format()` ou `%`, moins utilisés aujourd'hui, mais toujours valides spécialement pour des versions antérieures de Python.

Au niveau de l'implémentation, la performance de la concaténation a été améliorée dans les dernières versions de Python par l'optimisation de l'allocation de mémoire pour les opérations de chaînes fréquentes.

## Voir Aussi
- La documentation officielle sur les chaînes de caractères en Python : [docs.python.org](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str)
- PEP 498 sur les f-strings : [PEP 498](https://www.python.org/dev/peps/pep-0498/)
