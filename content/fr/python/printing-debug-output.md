---
title:                "Python: Afficher les sorties de débogage"
simple_title:         "Afficher les sorties de débogage"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous écrivez du code en Python, vous pouvez parfois rencontrer des erreurs ou des bugs qui peuvent sembler difficiles à comprendre. Dans ces cas-là, il peut être utile d'imprimer du texte de débogage pour mieux comprendre ce qui se passe dans votre programme.

## Comment faire

Utiliser la fonction `print()` en Python est une méthode simple et efficace pour afficher des messages de débogage dans votre code. Voici un exemple de code et de sortie :

```Python
# Exemple de code avec débogage
def calculate_sum(a, b):
    print("Calculating sum...")
    print("Value of a:", a)
    print("Value of b:", b)
    return a + b

# Appel de la fonction et affichage du résultat
print("The sum is:", calculate_sum(5, 3))
```

Sortie :

```
Calculating sum...
Value of a: 5
Value of b: 3
The sum is: 8
```

## Plongée en profondeur

Il y a plusieurs avantages à utiliser des messages de débogage dans votre code :

- Comprendre comment le code s'exécute : en affichant des messages à des endroits précis de votre code, vous pouvez suivre l'exécution et voir si des valeurs sont modifiées ou si des conditions sont remplies.
- Déterminer l'emplacement d'une erreur : si votre programme lance une erreur ou ne produit pas le résultat attendu, les messages de débogage peuvent vous aider à identifier où se situe le problème.
- Suivre la progression du code : si votre code prend du temps à s'exécuter, vous pouvez utiliser des messages de débogage pour suivre l'avancement et voir à quel point votre programme est efficace.

Cependant, il est important de retirer ces messages de débogage une fois que vous avez résolu tous les problèmes dans votre code, car ils peuvent ralentir l'exécution.

## Voir aussi

- [Documentation officielle Python sur la fonction `print()`](https://docs.python.org/fr/3/library/functions.html#print)
- [Article sur l'utilisation des messages de débogage en Python](https://realpython.com/python-debugging-pdb/)
- [Tutoriel vidéo sur les messages de débogage en Python](https://www.youtube.com/watch?v=QxgQ7Zuh6fQ)