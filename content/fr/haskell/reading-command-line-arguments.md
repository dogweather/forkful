---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Haskell: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi 

Si vous êtes un programmeur Haskell, il est fort probable que vous ayez déjà entendu parler de la lecture des arguments de ligne de commande. Mais si vous êtes nouveau dans le monde de la programmation fonctionnelle, vous pourriez vous demander pourquoi vous devriez vous intéresser à cela. En bref, la lecture des arguments de ligne de commande peut vous aider à rendre vos programmes plus dynamiques et interactifs en permettant à l'utilisateur de saisir des valeurs pour modifier le comportement du programme. 

## Comment Faire 

Pour commencer, nous allons créer un nouveau fichier nommé "args.hs" et nous allons le compiler en utilisant la commande "ghc" dans votre terminal. Vous pouvez trouver cette commande dans votre répertoire Haskell en tapant "stack exec ghc -- args.hs". Maintenant, nous allons écrire notre code dans le fichier "args.hs" en utilisant la syntaxe suivante: 

```Haskell 
import System.Environment 

main = do 
  args <- getArgs 
  print args
``` 

Ensuite, vous pouvez compiler le fichier en utilisant la commande "ghc" et exécuter le programme en utilisant "./args" suivi de quelques arguments que vous voulez tester. Par exemple, si vous tapez "./args Hello World", la sortie sera ["Hello", "World"]. Vous pouvez également utiliser des types spécifiques pour lire les arguments en utilisant "Data.Text" ou "Data.Text.IO" au lieu de "print". 

## Plongée Profonde 

Pour comprendre plus en détail comment la lecture des arguments de ligne de commande fonctionne en Haskell, il est important de comprendre les concepts de "IO" et de "monade". En bref, les programmes Haskell sont généralement purs, ce qui signifie qu'ils ne modifient pas l'état global du système. Cependant, en utilisant la monade "IO", vous pouvez exécuter des actions impures, telles que la lecture des arguments de la ligne de commande. 

## Voir Aussi 

- [Tutoriel Haskell: Lecture des Arguments de Ligne de Commande](https://www.tutorialspoint.com/haskell/haskell_command_line_arguments.htm) 
- [Documentation officielle de Haskell: System.Environment module](https://www.haskell.org/onlinereport/modules/System.Environment.html) 
- [Vidéo Youtube: Introduction à la Programmation Haskell - Les Types et la Lecture des Arguments de la Ligne de Commande](https://www.youtube.com/watch?v=OvdDpu-dwhY)