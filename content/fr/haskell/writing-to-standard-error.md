---
title:    "Haskell: Écrire vers l'erreur standard"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Ecrire dans la sortie d'erreur standard (standard error) est un moyen utile de signaler des erreurs et des avertissements lors de l'exécution d'un programme Haskell. Cela permet aux développeurs de comprendre rapidement ce qui s'est mal passé et d'apporter des corrections rapides.

## Comment Faire

Ci-dessous se trouve un exemple de code Haskell montrant comment écrire dans la sortie d'erreur standard :

```Haskell
-- Importer le module System.IO pour utiliser la fonction hPutStr
import System.IO

-- La fonction main qui contiendra notre code
main = do
    -- Ecrire une chaîne de caractères dans la sortie d'erreur standard
    hPutStr stderr "Une erreur s'est produite !"

    -- Utiliser la fonction error pour générer une erreur
    error "Une autre erreur est survenue."
```

En exécutant ce code, vous obtiendrez une sortie qui ressemble à ceci :

```
Une erreur s'est produite !*** Exception: Une autre erreur est survenue.
```

Comme vous pouvez le voir, la chaîne de caractères que nous avons écrite via la fonction hPutStr a été imprimée avant que l'erreur ne soit générée. Cela peut être très utile pour identifier la source d'une erreur.

## Plongée en profondeur

Il est important de noter que la sortie d'erreur standard n'est pas la même chose que la sortie standard (standard output). Alors que la sortie standard est généralement utilisée pour l'affichage de résultats ou informations utiles, la sortie d'erreur standard est spécifiquement destinée à signaler des erreurs et des avertissements.

En utilisant la fonction hPutStr pour écrire dans la sortie d'erreur standard, vous pouvez également spécifier un fichier de sortie en plus de la sortie par défaut sur la console. Cela peut être utile pour enregistrer les erreurs et les avertissements dans un fichier pour les consulter ultérieurement.

## Voir aussi

Pour en savoir plus sur la gestion des erreurs en Haskell, consultez ces liens utiles :

- Documentation officielle de Haskell sur la gestion des erreurs : https://www.haskell.org/documentation/99.0.1/libraries/base/Control.Exception.html
- Tutoriel sur la manipulation des erreurs en Haskell : https://wiki.haskell.org/Error_handling
- Exemples pratiques de gestion des erreurs en Haskell : https://www.codewars.com/kata/haskell-error-handling-practices