---
title:                "Affichage des sorties de débogage"
html_title:           "Python: Affichage des sorties de débogage"
simple_title:         "Affichage des sorties de débogage"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Il y a plusieurs raisons pour lesquelles vous pourriez vouloir utiliser l'impression de messages de débogage dans votre code Python. Cela peut vous aider à comprendre pourquoi votre programme ne fonctionne pas correctement, à identifier les erreurs et à vérifier les valeurs de vos variables à différents points d'exécution.

## Comment Faire

Voici un exemple de code Python qui montre comment imprimer un message de débogage à l'aide de la fonction `print()` :

```Python
x = 5
y = 10
print("La valeur de x est :", x)
print("La valeur de y est :", y)
```

Le résultat de ce code sera :

```
La valeur de x est : 5
La valeur de y est : 10
```

Comme vous pouvez le voir, en utilisant la fonction `print()` avec un message explicite, vous pouvez facilement vérifier la valeur de vos variables à des points spécifiques de votre code. Vous pouvez également utiliser des expressions ou des variables dans vos messages de débogage pour des informations plus détaillées. Par exemple :

```Python
x = 5
y = 10
print("La somme de", x, "et", y, "est égale à", x + y)
```

## Plongée en Profondeur

L'impression de messages de débogage peut également être utile pour suivre le flux d'exécution de votre code et identifier les erreurs. Vous pouvez utiliser la fonction `print()` pour imprimer des messages avant et après certaines parties de votre code pour voir si elles sont exécutées correctement. Par exemple :

```Python
x = 5
y = 10
print("Début du programme")
print("La valeur de x est :", x)
print("La valeur de y est :", y)
print("Addition de x et y :", x + y)
print("Fin du programme")
```

Le résultat de ce code sera :

```
Début du programme
La valeur de x est : 5
La valeur de y est : 10
Addition de x et y : 15
Fin du programme
```

Cela peut vous aider à localiser l'emplacement d'une erreur ou à vérifier si une partie de votre code n'est pas exécutée.

## Voir aussi

Pour en savoir plus sur l'impression de messages de débogage en Python, vous pouvez consulter les ressources suivantes :

- [La documentation officielle de Python sur la fonction `print()`](https://docs.python.org/fr/3/library/functions.html#print)
- [Un tutoriel sur le débogage en Python](https://realpython.com/python-debugging-pdb/)
- [Un guide sur l'utilisation des messages de débogage efficacement](https://www.bogotobogo.com/python/python_debugging_using_logging_module.php)