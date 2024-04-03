---
date: 2024-01-20 17:46:11.015511-07:00
description: "Extraire des sous-cha\xEEnes, c'est r\xE9cup\xE9rer des morceaux sp\xE9\
  cifiques d'une cha\xEEne de caract\xE8res. On le fait pour analyser, manipuler ou\
  \ transformer des\u2026"
lastmod: '2024-03-13T22:44:57.223452-06:00'
model: gpt-4-1106-preview
summary: "Extraire des sous-cha\xEEnes, c'est r\xE9cup\xE9rer des morceaux sp\xE9\
  cifiques d'une cha\xEEne de caract\xE8res."
title: "Extraction de sous-cha\xEEnes"
weight: 6
---

## Comment faire :
```Python
texte = "utilisateur@example.com"
utilisateur = texte[:texte.index('@')]
domaine = texte[texte.index('@')+1:]

print(utilisateur)  # Output: utilisateur
print(domaine)      # Output: example.com

# Autre méthode avec split
parties = texte.split('@')
utilisateur, domaine = parties[0], parties[1]

print(utilisateur)  # Encore: utilisateur
print(domaine)      # Et encore: example.com
```

## Plongée en profondeur
Historiquement, l'extraction de sous-chaînes était plus compliquée en Python. Avec l'évolution du langage, des méthodes comme `split()` et la compréhension des slices ont rendu cette tâche plus intuitive. Alternative aux slices, les expressions régulières (regex) permettent d'extraire des sous-chaînes avec des règles de correspondance complexes, mais elles sont plus lentes et plus verbeuses pour des cas simples. Niveau implémentation, Python utilise des objets chaînes immuables, donc chaque extraction crée un nouvel objet, ce qui a un impact sur la mémoire et la performance si pas géré avec soin.

## Voir aussi
- Documentation officielle Python sur les chaînes de caractères : https://docs.python.org/3/library/string.html
- Tutoriel sur les expressions régulières en Python : https://docs.python.org/3/howto/regex.html
- Guide approfondi sur les slices en Python : https://docs.python.org/3/reference/expressions.html#slicings
