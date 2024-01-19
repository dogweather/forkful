---
title:                "Supprimer les caractères correspondant à un modèle"
html_title:           "Ruby: Supprimer les caractères correspondant à un modèle"
simple_title:         "Supprimer les caractères correspondant à un modèle"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

---
## Qu'est-ce que c'est et pourquoi?
Supprimer des caractères correspondant à un motif est une tâche courante en programmation. Ça nous permet de manipuler et de nettoyer les données, essentiel pour manipuler les chaînes de caractères en Python.

## Comment faire:
Nous allons utiliser la méthode `translate()` combinée avec la méthode `maketrans()`. Veuillez regarder l'exemple ci-dessous :

```Python 
s = "ABCABCABC"
print(s.translate(str.maketrans('', '', 'B')))
```

Résultat : 

```
ACAACA
```

Vous pouvez voir comment toutes les instances de "B" ont été supprimées de la chaîne de caractères.

## Examens plus Approfondis
Avant l'existence de la méthode `translate()`, les développeurs Python utilisaient souvent des boucles pour supprimer les caractères correspondant à un motif, ce qui était beaucoup moins efficace.

Une alternative au `translate()` est l'utilisation de expressions régulières à l'aide du module `re.sub()`. Cependant, `translate()` est plus rapide pour les suppressions simples de caractères.

Au niveau de l'implémentation, `translate()` crée un tableau de correspondance qui est utilisé pour remplacer les caractères nécessaires dans la chaîne de caractères.

## Voir Aussi
Jetez un œil à ces ressources pour en savoir plus :

1. La documentation Python sur la méthode `translate()` (https://docs.python.org/fr/3/library/stdtypes.html#str.translate)
2. Un tutoriel utile sur la manipulation de chaînes de caractères en Python (https://realpython.com/python-strings/)
3. Un bref aperçu de la méthode `maketrans()` (https://www.programiz.com/python-programming/methods/string/maketrans)