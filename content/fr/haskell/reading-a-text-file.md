---
title:                "Haskell: Lecture d'un fichier texte"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi 

Si vous êtes un programmeur débutant ou expérimenté en Haskell, vous savez certainement à quel point la manipulation de fichiers est importante dans tout type d'application. Lire un fichier texte peut sembler simple, mais il y a plusieurs façons de le faire en Haskell et cela peut être une compétence très utile à ajouter à votre boîte à outils de programmation.

## Comment faire 

Pour lire un fichier texte en Haskell, nous allons utiliser la fonction "readFile" qui est intégrée dans le module "System.IO". Voici un exemple de code montrant comment utiliser cette fonction pour lire un fichier texte:

```
import System.IO

main = do
  contenu <- readFile "mon_fichier.txt"
  putStr contenu
```

Dans cet exemple, nous importons d'abord le module "System.IO" qui contient la fonction "readFile". Ensuite, nous utilisons la fonction "do" pour indiquer que nous allons exécuter plusieurs instructions. La première instruction utilise la fonction "readFile" pour lire le contenu du fichier "mon_fichier.txt" et le stocker dans la variable "contenu". Enfin, nous utilisons la fonction "putStr" pour afficher le contenu du fichier à l'écran.

Maintenant, voyons à quoi ressemble le contenu de notre fichier "mon_fichier.txt" et le résultat affiché à l'écran:

Contenu du fichier "mon_fichier.txt":
```
Bonjour, ceci est un fichier texte!
```

Résultat à l'écran:
```
Bonjour, ceci est un fichier texte!
```

Comme vous pouvez le voir, la fonction "readFile" lit le contenu du fichier et le stocke dans une chaîne de caractères. Nous pouvons ensuite utiliser cette chaîne de caractères pour effectuer des opérations telles que l'afficher à l'écran.

## Plongée en profondeur 

La fonction "readFile" peut sembler simple, mais elle utilise en fait plusieurs fonctions internes pour fonctionner. Par exemple, elle utilise la fonction "openFile" pour ouvrir le fichier, la fonction "hGetContents" pour lire le contenu du fichier et la fonction "hClose" pour fermer le fichier après avoir terminé de le lire. Si vous êtes curieux, vous pouvez jeter un coup d'œil à la documentation officielle pour en savoir plus sur ces fonctions et comment elles fonctionnent ensemble.

Maintenant que vous savez comment utiliser la fonction "readFile" pour lire un fichier texte, vous pouvez l'appliquer à vos propres projets et expérimenter avec différentes fonctions pour manipuler les données lues à partir du fichier.

## Voir aussi 

- La documentation officielle de Haskell pour en savoir plus sur la fonction "readFile": https://www.haskell.org/documentation/ 
- Un tutoriel complet sur la manipulation de fichiers en Haskell: https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Simple%20examples#reading-from-a-text-file