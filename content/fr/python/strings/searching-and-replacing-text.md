---
date: 2024-01-20 17:58:31.337986-07:00
description: 'Comment faire : Voici un petit script Python qui recherche et remplace
  du texte .'
lastmod: '2024-04-05T21:53:58.802819-06:00'
model: gpt-4-1106-preview
summary: Voici un petit script Python qui recherche et remplace du texte .
title: Recherche et remplacement de texte
weight: 10
---

## Comment faire :
Voici un petit script Python qui recherche et remplace du texte :

```python
texte_original = "Bonjour, le monde!"
texte_modifie = texte_original.replace("le monde", "tout le monde")
print(texte_modifie)
```

Résultat de l'exécution :

```
Bonjour, tout le monde!
```

Pour des cas plus complexes nécessitant des motifs spécifiques, on utilise des expressions régulières :

```python
import re

texte_original = "Les numéros: 1234, 5678, 91011"
pattern = re.compile(r"\d+")
texte_modifie = pattern.sub("NUM", texte_original)
print(texte_modifie)
```

Résultat :

```
Les numéros: NUM, NUM, NUM
```

## Plongée Profonde
Historiquement, la recherche et le remplacement de texte existent depuis l'avènement des éditeurs de texte. Outils simples mais puissants, ils sont devenus plus sophistiqués avec les expressions régulières, introduites dans les années 1950. En Python, on peut faire une recherche/remplacement simple avec la méthode `.replace()`, ou utiliser le module `re` pour de la manipulation avancée avec regex. Alternativement, des bibliothèques comme `str.replace()` ou `re.sub()` peuvent être utilisées en fonction du contexte et des besoins spécifiques.

## Voir Aussi
- Documentation Python sur les expressions régulières : https://docs.python.org/3/library/re.html
- Tutoriel sur la manipulation de chaînes de caractères en Python : https://realpython.com/python-strings/
- Guide sur les expressions régulières : https://www.regular-expressions.info/
