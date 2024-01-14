---
title:                "Python: Écrire vers l'erreur standard"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Écrire vers la sortie d'erreur standard, également connu sous le nom de *stderr*, est une compétence importante à acquérir en tant que développeur Python. Cela peut sembler intimidant au début, mais cela peut rendre le débogage et la gestion des erreurs plus efficaces.

## Comment faire

Pour écrire vers la sortie d'erreur standard en Python, vous pouvez utiliser la fonction `sys.stderr.write()` qui prend en paramètre une chaîne de caractères à écrire. Voici un exemple de code :

```Python
import sys

sys.stderr.write("Oops, une erreur s'est produite !\n")
```
La sortie de ce code sera :

```
Oops, une erreur s'est produite !
```

## Plongée en profondeur

En écrivant vers la sortie d'erreur standard, vous pouvez ajouter des informations supplémentaires sur une erreur pour faciliter le processus de débogage. Vous pouvez également utiliser la fonction `sys.stderr.flush()` pour vider immédiatement le contenu de la mémoire tampon vers la sortie d'erreur standard.

De plus, il est possible d'utiliser la redirection de sortie pour enregistrer les erreurs dans un fichier plutôt que de les afficher à l'écran. Pour ce faire, vous pouvez utiliser `sys.stderr = open("erreurs.log", "w")` pour rediriger la sortie d'erreur standard vers un fichier nommé "erreurs.log".

## Voir aussi

Pour en savoir plus sur la gestion des erreurs en Python, vous pouvez consulter ces ressources :

- [Didacticiel sur les erreurs et les exceptions en Python](https://realpython.com/python-exceptions/)
- [Documentation officielle sur la sortie d'erreur standard en Python](https://docs.python.org/fr/3/library/sys.html#sys.stderr)
- [Référence sur la gestion des erreurs en Python](https://docs.python.org/fr/3/tutorial/errors.html)

Maintenant, vous êtes prêt à écrire vers la sortie d'erreur standard en Python ! Utilisez cette compétence pour améliorer votre code et faciliter le processus de débogage. Bon codage !