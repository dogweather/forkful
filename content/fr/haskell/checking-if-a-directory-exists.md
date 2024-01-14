---
title:    "Haskell: Vérifier si un répertoire existe"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Avez-vous déjà eu besoin de vérifier si un répertoire existe dans votre code Haskell ? Peut-être que vous avez écrit un programme qui doit créer de nouveaux fichiers et vous souhaitez vous assurer que le répertoire dans lequel vous les enregistrez existe déjà. Ou peut-être que vous avez besoin de manipuler des fichiers dans un répertoire spécifique et vous voulez vous assurer qu'il existe avant de continuer. Dans cet article, nous allons découvrir comment vérifier si un répertoire existe en utilisant Haskell.

## Comment faire

Heureusement, il existe une fonction intégrée dans la bibliothèque standard de Haskell qui nous permet de vérifier si un répertoire existe. Il s'agit de la fonction `doesDirectoryExist` du module `System.Directory`. Pour l'utiliser, nous devons d'abord importer le module :

```Haskell
import System.Directory
```

Ensuite, nous pouvons appeler la fonction `doesDirectoryExist` et lui passer le chemin du répertoire en tant qu'argument. Cela peut être un chemin absolu ou relatif :

```Haskell
-- Vérifier si le répertoire "exemple" existe dans le répertoire actuel
doesDirectoryExist "exemple"

-- Vérifier si le répertoire "/chemin/absolu/vers/mon/répertoire" existe
doesDirectoryExist "/chemin/absolu/vers/mon/répertoire"
```

La fonction renverra un résultat de type `IO Bool`, ce qui signifie qu'elle est effectuée dans le contexte de l'entrée/sortie et qu'elle renverra soit `True` si le répertoire existe, soit `False` s'il n'existe pas. Voyons un exemple de sortie en utilisant l'interpréteur GHCi :

```Haskell
λ> doesDirectoryExist "/etc"
True
λ> doesDirectoryExist "inexistant"
False
```

Maintenant que nous savons comment utiliser la fonction, nous pouvons l'intégrer dans notre code pour prendre des décisions en fonction de l'existence ou non d'un répertoire. Par exemple, nous pouvons utiliser la combinaison des fonctions `doesDirectoryExist` et `createDirectory` pour créer un nouveau répertoire uniquement s'il n'existe pas déjà :

```Haskell
-- Vérifie si le répertoire "nouveau_repertoire" existe
directoryExists <- doesDirectoryExist "nouveau_repertoire"

-- Créer un nouveau répertoire uniquement s'il n'existe pas déjà
unless directoryExists $ createDirectory "nouveau_repertoire"
```

## Plongée en profondeur

Si vous souhaitez en savoir plus sur la manière dont la fonction `doesDirectoryExist` fonctionne en interne, vous pouvez consulter sa définition dans la documentation de Haskell. Il s'agit essentiellement d'une interface pour appeler des fonctions système qui vérifient si un chemin de fichier existe.

Il est également intéressant de noter que la fonction `doesDirectoryExist` est spécifique à la plate-forme. Cela signifie que son comportement peut différer selon le système d'exploitation sur lequel vous exécutez votre code. Par exemple, si vous utilisez Windows, il vérifiera si le chemin fourni correspond à un répertoire, tandis que si vous utilisez Linux, il vérifiera si le chemin correspond à un répertoire ou à un lien symbolique qui pointe vers un répertoire.

## Voir aussi

Voici quelques ressources supplémentaires que vous pourriez trouver utiles :

- [Documentation officielle de la fonction `doesDirectoryExist`](https://hackage.haskell.org/package/directory/docs/System-Directory.html#v:doesDirectoryExist)
- [Documentation du module `System.Directory`](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Tutoriel sur la manipulation des fichiers et des répertoires en Haskell](https://dev.to/thomas/know-your-io-filesystem-in-haskell-3fln) (en anglais)