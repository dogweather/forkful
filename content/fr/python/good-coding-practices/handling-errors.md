---
title:                "Gestion des erreurs"
aliases:
- /fr/python/handling-errors.md
date:                  2024-01-26T00:56:07.758527-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gestion des erreurs"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/handling-errors.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

La gestion des erreurs en Python (ou dans tout autre langage de programmation) consiste à anticiper l'inattendu – c'est l'art de gérer avec grâce lorsque les choses prennent une mauvaise tournure dans votre code. Nous le faisons pour éviter les plantages, guider les utilisateurs et rendre nos programmes robustes et fiables.

## Comment faire :

``` Python
# Bloc try-except de base
try:
    # code à risque
    number = int(input("Entrez un nombre : "))
except ValueError:
    # gestion de l'erreur
    print("Ce n'est pas un nombre !")

# Spécification de multiples exceptions
try:
    # code susceptible de lever différentes exceptions
    result = 10 / int(input("Entrez un diviseur : "))
except ZeroDivisionError:
    print("Oups ! On ne peut pas diviser par zéro.")
except ValueError:
    print("Il me faut un nombre, mon pote.")

# Utilisation de else et finally
try:
    number = int(input("Entrez un nombre pour le carré : "))
except ValueError:
    print("J'ai dit un nombre !")
else:
    # aucune erreur survenue
    print("Le carré de votre nombre est : ", number**2)
finally:
    # exécute toujours
    print("Merci d'avoir essayé !")
```

Exemple de sortie lorsque l'on entre un nombre invalide pour le premier bloc :
```
Entrez un nombre : bonjour
Ce n'est pas un nombre !
```

## Exploration approfondie

Depuis l'aube de la programmation, la gestion des erreurs a été cruciale. Les premières approches étaient rudimentaires, comme vérifier les conditions avant chaque opération à risque. La syntaxe `try-except` de Python est issue d'un héritage de gestion des exceptions dans des langues plus anciennes comme C++ et Java, simplifiant le processus.

Lorsque vous essayez (`try`) un bloc de code, Python surveille les exceptions potentielles. Si une erreur survient, le bloc `except` l'attrape. Vous pouvez être précis sur les exceptions que vous attrapez ou toutes les attraper avec un `except` nu. Cependant, la spécificité en premier est la meilleure approche – elle est précise et non un filet attrape-tout.

`else` et `finally` sont des extras dans ce concept. Le bloc `else` s'exécute si le bloc try est exempt d'erreur.  `finally` est l'ami fiable qui s'exécute quoi qu'il arrive – pensez aux opérations de nettoyage.

Des alternatives ? Il y en a certainement. Certaines langues utilisent des codes de retour au lieu d'exceptions. Vous pourriez également rencontrer des instructions `with` pour la gestion des ressources ou des `assertions` qui vérifient les conditions pendant le développement. Mais quand nous parlons de stratégies solides de gestion des erreurs, le modèle try-catch se distingue par sa lisibilité et sa structure.

## Voir Aussi

Voici quelques bonnes ressources supplémentaires pour plonger encore plus profondément :

- La documentation officielle de Python sur les erreurs et les exceptions : [Python Docs – Errors and Exceptions](https://docs.python.org/3/tutorial/errors.html)
- Le guide de Real Python sur le sujet : [Real Python - Le bloc try/except/else/finally](https://realpython.com/python-exceptions/)
- Une discussion réfléchie sur les meilleures pratiques de gestion des erreurs : [Stack Overflow – Comment ignorer correctement les exceptions ?](https://stackoverflow.com/questions/4990718/about-catching-any-exception)
