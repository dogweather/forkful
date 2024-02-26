---
date: 2024-01-20 17:43:00.934252-07:00
description: "Supprimer des caract\xE8res selon un motif, c'est retirer des s\xE9\
  quences sp\xE9cifiques dans une cha\xEEne. Les programmeurs le font pour nettoyer\
  \ des donn\xE9es,\u2026"
lastmod: '2024-02-25T18:49:54.111634-07:00'
model: gpt-4-1106-preview
summary: "Supprimer des caract\xE8res selon un motif, c'est retirer des s\xE9quences\
  \ sp\xE9cifiques dans une cha\xEEne. Les programmeurs le font pour nettoyer des\
  \ donn\xE9es,\u2026"
title: "Suppression de caract\xE8res correspondant \xE0 un motif"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Supprimer des caractères selon un motif, c'est retirer des séquences spécifiques dans une chaîne. Les programmeurs le font pour nettoyer des données, valider des entrées, ou manipuler du texte efficacement.

## Comment faire :
```python
import re

# Exemple 1: Supprimer tous les chiffres d'une chaîne
texte = "Paris75000"
resultat = re.sub(r'\d', '', texte)
print(resultat)  # Paris

# Exemple 2: Supprimer les caractères spéciaux
texte = "Bonjour! Comment ça va? #soleil"
resultat = re.sub(r'[!#?]', '', texte)
print(resultat)  # Bonjour Comment ça va soleil

# Exemple 3: Utilisation de la méthode translate
texte = "Il fait beau! 20°C aujourd'hui."
a_supprimer = dict.fromkeys(map(ord, '°!'), None)
resultat = texte.translate(a_supprimer)
print(resultat)  # Il fait beau 20C aujourd'hui.
```

## Plongée en profondeur
Historiquement, les expressions régulières sont un outil utilisé en informatique depuis les années 1950. Elles permettent de décrire des motifs de chaînes de caractères. En Python, le module `re` nous offre cette capacité.

Il existe d'autres méthodes pour supprimer des caractères, comme `str.replace()` ou `str.strip()`, mais elles sont plus limitées car elles ne gèrent pas les motifs.

Quand on utilise `re.sub()`, Python doit d'abord compiler le motif, puis l'appliquer à la chaîne. La méthode `translate()` peut être plus rapide pour des suppressions simples car elle évite cette compilation, mais elle est moins flexible.

## Voir aussi
- Documentation officielle Python sur les expressions régulières : [Expressions régulières](https://docs.python.org/3/library/re.html)
- Tutoriel Python sur le traitement des chaînes de caractères : [Manipulation de texte](https://docs.python.org/3/howto/regex.html#regex-howto)
- Comparaison de performances entre différentes méthodes de suppression de caractères : [Performances str.replace vs re.sub](https://stackoverflow.com/questions/3411771/best-way-to-replace-multiple-characters-in-a-string)
