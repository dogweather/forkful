---
title:    "Haskell: Vérification de l'existence d'un répertoire"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes développeur en Haskell, il est important de connaître les fonctionnalités de base de ce langage de programmation fonctionnel. L'une de ces fonctionnalités est la vérification de l'existence d'un répertoire. Dans cet article, nous allons expliquer pourquoi il est important de vérifier si un répertoire existe et comment le faire efficacement en Haskell.

## Comment faire

Pour vérifier si un répertoire existe en Haskell, nous allons utiliser la fonction `doesDirectoryExist` de la bibliothèque `System.Directory`. Cette fonction prend en paramètre un chemin de fichier et retourne un `Bool` qui indique si le répertoire existe ou non. Voici un exemple de code qui vérifie si un répertoire nommé "mon_repetoire" existe :

```Haskell
import System.Directory

main = do
    dirExists <- doesDirectoryExist "mon_repetoire"
    if dirExists
        then putStrLn "Le repertoire existe."
        else putStrLn "Le repertoire n'existe pas."
```

La sortie de ce code dépendra de l'existence du répertoire "mon_repetoire" :

```
Le répertoire existe.
```

ou

```
Le répertoire n'existe pas.
```

## Plongée en profondeur

Pour comprendre comment la fonction `doesDirectoryExist` fonctionne, il est important de comprendre comment Haskell gère les erreurs. En Haskell, les fonctions peuvent retourner un type `Maybe` pour traiter les erreurs. `Maybe` est un type de données qui peut contenir une valeur ou rien (`Nothing`). Dans le cas de `doesDirectoryExist`, la fonction retourne un `Just True` si le répertoire existe et `Just False` s'il n'existe pas. Si une erreur se produit lors de la vérification, la fonction retournera `Nothing`.

Il est également important de noter que la fonction `doesDirectoryExist` effectue uniquement une vérification de l'existence du répertoire et ne renvoie pas d'informations sur les permissions d'accès.

## Voir aussi

- [Documentation complète de la fonction `doesDirectoryExist`](https://hackage.haskell.org/package/directory/docs/System-Directory.html#v:doesDirectoryExist)
- [Tutoriel sur la gestion des erreurs en Haskell](https://wiki.haskell.org/Error_handling)
- [Guide complet pour débuter en programmation Haskell](https://wiki.haskell.org/Learn_Haskell)