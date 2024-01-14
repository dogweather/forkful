---
title:    "Haskell: Lecture d'un fichier texte."
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur Haskell, vous savez sûrement déjà que la lecture de fichiers texte est une tâche courante et importante. Mais si vous êtes novice en Haskell ou en programmation en général, vous pourriez vous demander pourquoi quelqu'un voudrait lire un fichier texte en premier lieu. La réponse est simple : les fichiers texte contiennent souvent des informations utiles, telles que des données de base de données, des configurations de programme ou même du contenu de pages web. En apprenant à lire des fichiers texte, vous pourrez automatiser ces processus et manipuler les données à votre guise.

## Comment faire

Maintenant que vous savez pourquoi la lecture de fichiers texte est importante, il est temps de voir comment le faire en Haskell. Tout d'abord, nous devons ouvrir le fichier que nous voulons lire en utilisant la fonction `openFile` avec le mode d'ouverture `ReadMode`. Nous pouvons ensuite utiliser la fonction `hGetContents` pour lire le contenu du fichier en tant que chaîne de caractères :

```Haskell
import System.IO

main = do
    handle <- openFile "mon_fichier.txt" ReadMode
    contenu <- hGetContents handle
    print contenu
```

Dans cet exemple, nous utilisons `main` pour définir une action IO qui sera exécutée lorsque le programme sera lancé. Nous ouvrons le fichier "mon_fichier.txt" en mode lecture et stockons le gestionnaire de fichier dans la variable `handle`. Ensuite, nous utilisons `hGetContents` pour lire le contenu du fichier dans la variable `contenu`. Enfin, nous imprimons le contenu du fichier à l'aide de la fonction `print`. Vous pouvez également utiliser d'autres fonctions pour manipuler le contenu du fichier comme vous le souhaitez.

## Deep Dive

Maintenant que vous savez comment lire un fichier texte en Haskell, vous pourriez vous demander ce qui se passe réellement lorsque vous utilisez `openFile` et `hGetContents`. En bref, `openFile` ouvre une poignée de fichiers qui peut ensuite être utilisée pour exécuter différentes actions sur le fichier, telles que la lecture ou l'écriture. `hGetContents` utilise cette poignée pour lire le contenu du fichier et le stocker dans une chaîne de caractères. Cela signifie que vous pouvez utiliser la même poignée pour effectuer plusieurs actions sur le fichier.

## Voir aussi

- [Documentation officielle de la bibliothèque System.IO](https://www.haskell.org/onlinereport/standard-prelude.html#module-system-io)
- [Guide de lecture de fichiers en Haskell](https://wiki.haskell.org/Introduction_to_IO/Reading_files)