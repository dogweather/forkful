---
title:                "Recherche et remplacement de texte"
html_title:           "Arduino: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?
Chercher et remplacer du texte signifie trouver certaines chaînes de caractères dans un texte et les remplacer par d'autres. Les programmeurs le font pour manipuler des données, automatiser des tâches répétitives et nettoyer des chaînes de caractères.

## Comment faire:
En Python, on utilise la méthode `replace()` pour chercher et remplacer du texte. Voici un exemple:

```Python
texte = "J'aime la pomme"
nouveau_texte = texte.replace("pomme", "banane")
print(nouveau_texte)
```

La sortie serait: 'J'aime la banane'.

## Approfondissement:
Historiquement, chercher et remplacer du texte est une fonction fondamentale dans les éditeurs de texte depuis les années 70. En Python, le module `re` offre une alternative avec plus de flexibilité, permettant des recherches et remplacements basés sur des expressions régulières.

```Python
import re
texte = "J'aime la pomme et la pomme"
nouveau_texte = re.sub("pomme", "banane", texte)
print(nouveau_texte)
```

La sortie: 'J'aime la banane et la banane'.

Notez que `replace()` ne prend pas en compte les majuscules et minuscules alors que `re.sub()` peut le faire avec l'argument `re.IGNORECASE`.

## Voir aussi:
Pour plus d'informations sur la méthode `replace()`, consultez: https://docs.python.org/3/library/stdtypes.html#str.replace 
Pour plus de détails sur `re.sub()`, visitez: https://docs.python.org/3/library/re.html#re.sub