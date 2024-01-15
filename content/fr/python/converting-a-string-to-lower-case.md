---
title:                "Conversion d'une chaîne de caractères en minuscules"
html_title:           "Python: Conversion d'une chaîne de caractères en minuscules"
simple_title:         "Conversion d'une chaîne de caractères en minuscules"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous avez peut-être remarqué que certaines fonctions Python utilisent des chaînes de caractères en minuscules, tandis que d'autres utilisent des chaînes de caractères en majuscules. Mais qu'est-ce qui pousse les programmeurs à vouloir convertir une chaîne de caractères en minuscules ? Eh bien, il y a quelques raisons.

La première raison est que les chaînes de caractères en minuscules sont généralement plus faciles à manipuler et à comparer. En effet, en minuscules, toutes les lettres ont la même taille et il n'y a pas de lettres "spéciales" comme les majuscules accentuées. De plus, certaines fonctions ne fonctionneront que si les chaînes de caractères sont en minuscules, donc il peut être nécessaire de les convertir pour utiliser ces fonctions.

La deuxième raison est que certaines langues sont plus largement utilisées en minuscules qu'en majuscules. Par exemple, en français, la plupart des textes sont écrits en minuscules, donc si vous travaillez avec du texte en français, il peut être utile de le convertir en minuscules pour une meilleure compréhension.

## Comment faire

Pour convertir une chaîne de caractères en minuscules en Python, il existe une fonction intégrée appelée `lower()`. Cette fonction prend une chaîne de caractères en paramètre et renvoie une nouvelle chaîne de caractères avec toutes les lettres en minuscules.

```Python
texte = "BONJOUR LES AMIS"
print(texte.lower())

# Output: bonjour les amis
```

Vous pouvez également utiliser la méthode `casefold()`, qui fonctionne de la même manière que la méthode `lower()`, mais elle est plus utile pour les chaînes de caractères dans des langues non latines.

```Python
titre = "ÉCRITURE CRÉATIVE"
print(titre.casefold())

# Output: écriture créative
```

Il est important de noter que ces fonctions ne modifient pas la chaîne de caractères d'origine, elles renvoient plutôt une nouvelle chaîne de caractères que vous pouvez assigner à une variable ou utiliser directement.

## Deep Dive

Maintenant que nous savons comment convertir une chaîne de caractères en minuscules, voyons un peu plus en détail ce qui se passe sous le capot.

Lorsque vous utilisez la méthode `lower()` ou `casefold()`, Python utilise en fait les méthodes `isupper()` et `lower()` pour chaque caractère de la chaîne de caractères d'origine. Si le caractère est en majuscule, il sera converti en minuscule et ajouté à la nouvelle chaîne de caractères. Sinon, il sera simplement ajouté tel quel.

Vous pouvez voir cela en action si vous regardez le code source de ces méthodes. Par exemple, voici le code source de la méthode `lower()` :

```Python
def lower(texte):
    texte_final = ""
    for caractere in texte:
        if caractere.isupper():
            texte_final += caractere.lower()
        else:
            texte_final += caractere
    
    return texte_final
```

Comme vous pouvez le voir, c'est assez simple mais efficace.

## Voir aussi

Vous pouvez en apprendre plus sur les chaînes de caractères en parcourant ces liens utiles :

- [Documentation sur les chaînes de caractères en Python](https://docs.python.org/fr/3/tutorial/introduction.html#strings)
- [Tutoriel sur les fonctions en Python](https://www.learnpython.org/fr/Functions)
- [Liste complète des méthodes de chaînes de caractères en Python](https://docs.python.org/fr/3/library/stdtypes.html#string-methods)