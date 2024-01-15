---
title:                "Utiliser les expressions régulières"
html_title:           "Haskell: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur en Haskell, vous connaissez sûrement l'importance des expressions régulières dans le traitement des données. Les expressions régulières sont des motifs de recherche permettant de trouver et de manipuler des chaînes de caractères de façon efficace. Engager dans l'utilisation des expressions régulières peut grandement améliorer votre productivité et simplifier vos tâches de manipulation de données.

## Comment faire

Pour utiliser les expressions régulières en Haskell, vous devez d'abord importer le module `Text.Regex.Posix`. Ensuite, vous pouvez utiliser la fonction `match` pour vérifier si une chaîne de caractères correspond à un motif régulier. Voici un exemple de code montrant comment utiliser les expressions régulières pour vérifier si une adresse email est valide :

```Haskell
import Text.Regex.Posix

isValidEmail :: String -> Bool
isValidEmail email = match emailRegex email
  where emailRegex = "[a-zA-Z0-9]+@[a-zA-Z0-9]+\\.com"

main :: IO ()
main = do
  putStrLn "Entrez une adresse email : "
  email <- getLine
  if isValidEmail email
    then putStrLn "Valide!"
    else putStrLn "Invalide."
```

Dans cet exemple, nous utilisons le motif `"[a-zA-Z0-9]+@[a-zA-Z0-9]+\\.com"` qui correspond à l'adresse email typique avec un nom d'utilisateur composé de lettres et de chiffres, suivi du symbole "@" et d'un domaine composé de lettres et de chiffres, suivi de l'extension ".com". Vous pouvez bien sûr utiliser des motifs plus complexes pour des validations plus précises.

## Plongée profonde

Haskell offre de nombreuses fonctions utiles pour manipuler des chaînes de caractères avec des expressions régulières. Par exemple, la fonction `subRegex` permet de remplacer une partie d'une chaîne de caractères correspondant à un motif par une autre chaîne de caractères. Voici un exemple de son utilisation :

```Haskell
import Text.Regex.Posix

main :: IO ()
main = do
  let sentence = "La pomme est délicieuse!"
  let modifiedSentence = subRegex (mkRegex "pomme") sentence "poire"
  putStrLn modifiedSentence
  -- Affiche : "La poire est délicieuse!"
```

Il existe également des fonctions pour extraire des sous-chaînes correspondants à un motif spécifique, pour diviser une chaîne de caractères en différents morceaux selon un motif, ou encore pour effectuer des remplacements avec prise en compte du contenu des sous-matches.

## Voir aussi

- [Documentation du module `Text.Regex.Posix`](https://hackage.haskell.org/package/regex-posix-0.95.2/docs/Text-Regex-Posix.html)
- [Tutorial sur les expressions régulières en Haskell](https://www.cs.helsinki.fi/u/phannahe/Compilers2011/metahaskel3.html)
- [Les principales fonctions pour manipuler les expressions régulières en Haskell](https://wiki.haskell.org/Regular_expressions)