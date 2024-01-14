---
title:    "Python: Écriture sur l'erreur standard"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation Python, il est souvent nécessaire d'afficher des informations sur l'exécution du code. Souvent, ces informations sont imprimées à la console à l'aide de la fonction `print`, mais que faire si l'on veut afficher un message d'erreur ? Dans ce cas, il est utile de savoir comment écrire vers le *standard error*.

## Comment faire

Ecrire vers le *standard error* est très similaire à l'écriture vers le *standard output*. La principale différence est l'utilisation de `sys.stderr` au lieu de `sys.stdout`. Voici un exemple de code :

```Python
import sys

print("Ceci sera imprimé à la console")
print("Ceci sera imprimé au *standard error*", file=sys.stderr)

# Output :
# Ceci sera imprimé à la console
```

Comme vous pouvez le constater, `sys.stderr` peut être utilisé avec la fonction `print` pour écrire vers le *standard error*. Cela peut être très utile lorsque vous avez besoin d'afficher des messages d'erreur pour le débogage de votre code.

## Plongée en profondeur

L'utilisation de `sys.stderr` pour écrire vers le *standard error* peut être utile dans plusieurs situations. Par exemple, si vous utilisez un module de traitement de fichier et qu'une erreur se produit, vous pouvez utiliser `sys.stderr` pour afficher un message d'erreur à l'utilisateur plutôt que de simplement planter votre programme.

Il est également possible d'utiliser `sys.stderr` avec la syntaxe `with` pour gérer les exceptions et afficher un message d'erreur approprié en cas d'erreur. Voici un exemple :

```Python
import sys

try:
    # Code qui peut générer une exception
    1/0
except ZeroDivisionError as error:
    # Affichage d'un message d'erreur vers le *standard error*
    print("Une erreur s'est produite :", error, file=sys.stderr)

# Output :
# Une erreur s'est produite : division by zero
```

En utilisant cette méthode, vous pouvez mieux contrôler les messages d'erreur et les rendre plus explicites pour l'utilisateur.

## Voir aussi

- [La documentation officielle de Python pour la gestion d'exceptions](https://docs.python.org/fr/3/tutorial/errors.html)
- [Un tutoriel sur l'utilisation de la syntaxe `with` pour la gestion des exceptions](https://realpython.com/lessons/using-python-with-sources-statements/)
- [Un article sur la différence entre le *standard output* et le *standard error*](https://www.tutorialspoint.com/what-is-the-difference-between-standard-error-and-standard-output-in-linux)