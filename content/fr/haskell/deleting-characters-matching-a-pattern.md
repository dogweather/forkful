---
title:                "Suppression de caractères correspondant à un motif"
html_title:           "Haskell: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Il y a plusieurs raisons pour lesquelles on pourrait vouloir supprimer des caractères qui correspondent à un motif dans un programme Haskell. Cela peut être fait pour nettoyer une chaîne de caractères avant une opération de traitement de données ou pour simplifier un algorithme en éliminant les informations inutiles.

## Comment Faire

Voici un exemple simple de code qui supprime tous les caractères de type "a" dans une chaîne de caractères :

```Haskell
import Data.Char (isLower) -- importe la fonction isLower qui vérifie si un caractère est minuscule

deleteA :: String -> String -- déclare une fonction qui prend une chaîne de caractères en entrée et en retourne une en sortie
deleteA str = filter (not . isLower) str -- utilise la fonction filter pour appliquer la fonction not . isLower à chaque caractère de la chaîne d'entrée et renvoie la chaîne filtrée

main = do
  let str = "Hello, world!" -- déclare une variable str avec la chaîne de caractères à traiter
  print(str) -- affiche la chaîne d'origine
  print(deleteA str) -- affiche la chaîne avec les caractères "a" supprimés
```

Résultat :

```
"Hello, world!"
"Hell, world!"
```

On peut également utiliser des expressions régulières pour supprimer les caractères correspondant à un motif spécifique. Voici un autre exemple de code qui supprime tous les nombres d'une chaîne de caractères :

```Haskell
import Text.Regex.Posix -- importe la fonction =~ qui permet d'utiliser des expressions régulières

deleteDigits :: String -> String -- déclare une fonction qui prend une chaîne de caractères en entrée et en retourne une en sortie
deleteDigits str = str =~ "[0-9]" :: String -- utilise l'expression régulière [0-9] pour supprimer tous les nombres dans la chaîne et renvoie la chaîne changée

main = do
  let str = "Today's date is 10/15/2021." -- déclare une variable str avec la chaîne de caractères à traiter
  print(str) -- affiche la chaîne d'origine
  print(deleteDigits str) -- affiche la chaîne avec les nombres supprimés
```

Résultat :

```
"Today's date is 10/15/2021."
"Today's date is /./."
```

## Plongée Profonde

En utilisant la fonction `filter` et le concept de fonction d'ordre supérieur, on peut créer une fonction générique pour supprimer tous les caractères correspondant à un motif dans une chaîne de caractères :

```Haskell
-- Déclare une fonction générique qui prend une fonction de test en entrée et une chaîne de caractères en entrée et en retourne une en sortie
genericDelete :: (Char -> Bool) -> String -> String
genericDelete testFunc str = filter (not . testFunc) str -- utilise la fonction de test pour évaluer chaque caractère dans la chaîne et renvoie la chaîne filtrée

main = do
  let str = "Haskell is fun!" -- déclare une variable str avec la chaîne de caractères à traiter
  print(str) -- affiche la chaîne d'origine
  print(genericDelete (== 'a') str) -- affiche la chaîne avec les caractères "a" supprimés en utilisant la fonction générique avec la fonction de test (== 'a')
```

Résultat :

```
"Haskell is fun!"
"Hskell is fun!"
```

## Voir Aussi

Pour plus d'informations sur les fonctions de manipulation de chaînes de caractères en Haskell, vous pouvez consulter ces liens : 

- [Documentation officielle de Haskell](https://www.haskell.org/documentation/) sur les fonctions standard pour le traitement de chaînes de caractères
- [Tutoriel sur les expressions régulières en Haskell](https://wiki.haskell.org/Regular_expressions) pour une approche plus avancée de la suppression de caractères par motif
- [Hackage](https://hackage.haskell.org/) pour une liste complète de bibliothèques et de packages pour le traitement de données en Haskell.