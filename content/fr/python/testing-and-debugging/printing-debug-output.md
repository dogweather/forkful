---
date: 2024-01-20 17:53:21.866227-07:00
description: "Le \"print debugging\" consiste \xE0 afficher des infos dans la console\
  \ pour comprendre ce qui se passe dans le code. On l'utilise parce que c'est simple\
  \ et\u2026"
lastmod: '2024-03-13T22:44:57.238902-06:00'
model: gpt-4-1106-preview
summary: "Le \"print debugging\" consiste \xE0 afficher des infos dans la console\
  \ pour comprendre ce qui se passe dans le code. On l'utilise parce que c'est simple\
  \ et\u2026"
title: "Affichage des sorties de d\xE9bogage"
weight: 33
---

# Debuggage avec print(): Comment ça marche?

## Quoi et pourquoi ?
Le "print debugging" consiste à afficher des infos dans la console pour comprendre ce qui se passe dans le code. On l'utilise parce que c'est simple et direct, pas besoin de configurer un environnement de débogage, et ça marche presque tout le temps.

## Comment ça marche ?
Utiliser `print()` en Python est enfantin. Regardons comment ça se passe avec des exemples simples.

```Python
# Exemple basique
variable = "debuggage"
print(variable)

# Ajout de contexte
nombre = 42
print(f"La valeur de nombre est: {nombre}")

# Print pour suivre le flux du programme
def ma_fonction(x):
    print(f"ma_fonction est appelée avec {x}")
    return x * 2

resultat = ma_fonction(3)
print(f"Résultat est {resultat}")
```

Ce qu'on obtient en sortie:

```
debuggage
La valeur de nombre est: 42
ma_fonction est appelée avec 3
Résultat est 6
```

## Deep Dive
Historiquement, `print()` a toujours été la méthode de débogage rapide. Avant même que les IDE modernes offrent des débogueurs complexes, imprimer des valeurs était la manière standard de comprendre les erreurs. Les alternatives incluent l'utilisation de débogueurs, qui permettent de mettre des points d'arrêt et d'examiner l'état du programme ligne par ligne, ou encore l'écriture de logs détaillés. En parlant d'implémentation, `print()` en Python 3 est une fonction alors qu'en Python 2, c'était une instruction - une petite mais importante différence! 

## Voir également
Pour aller plus loin avec le débogage:

- La doc officielle Python sur `print()`: https://docs.python.org/3/library/functions.html#print
- Tutoriel sur PDB, le débogueur de Python: https://docs.python.org/3/library/pdb.html
- Guide sur le logging en Python: https://docs.python.org/3/howto/logging.html
