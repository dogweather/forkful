---
title:                "Convertir une chaîne en minuscules"
html_title:           "Haskell: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur curieux qui aime explorer de nouvelles langues de programmation, alors la conversion d'une chaîne de caractères en minuscules pourrait être une tâche fascinante pour vous.

## Comment faire

```Haskell
import Data.Char (toLower)

-- Définition d'une fonction pour convertir une chaîne de caractères en minuscules
lowercase :: String -> String
lowercase str = map toLower str

-- Exemple d'utilisation 
main = do
  let str = "BONJOUR TOUT LE MONDE"
  putStrLn (lowercase str)
  -- Output : bonjour tout le monde
```

## Plongée en profondeur

La fonction de conversion en minuscules utilise la bibliothèque `Data.Char` et la fonction `toLower` pour parcourir la chaîne de caractères et remplacer chaque caractère par sa version en minuscules. Cela peut sembler simple, mais il y a plusieurs choses à considérer lors de la manipulation de chaînes de caractères, telles que la gestion des caractères spéciaux et l'utilisation de différentes bibliothèques pour des fonctionnalités avancées.

## Voir aussi
- [Documentation officielle de Haskell](https://www.haskell.org/documentation/)
- [Formation complète sur Haskell](https://www.coursera.org/learn/programming-languages)
- [Tutoriels interactifs en ligne](https://www.haskell.org/tutorial/)