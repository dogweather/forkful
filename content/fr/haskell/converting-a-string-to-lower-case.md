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

## Qu'est-ce que c'est et pourquoi le faire?
Convertissez votre chaîne de caractères en lettres minuscules est une tâche courante dans la programmation. Cela signifie simplement prendre une chaîne de caractères en entrée et la convertir en une nouvelle chaîne de caractères avec toutes les lettres en minuscules. Les programmeurs le font souvent pour faciliter la comparaison ou la manipulation de chaînes de caractères en utilisant des algorithmes ou des structures de données spécifiques.

## Comment faire:
Nous allons maintenant vous montrer comment convertir une chaîne de caractères en minuscules en utilisant Haskell. Voici un exemple de code que vous pouvez utiliser:

```Haskell
import Data.Char (toLower)

lowerCaseString :: String -> String
lowerCaseString = map toLower
```
L'importation de Data.Char est nécessaire pour accéder à la fonction toLower qui fait le travail de conversion. Ensuite, nous définissons notre propre fonction lowerCaseString en utilisant la fonction prédéfinie map qui applique la fonction toLower à chaque élément de la chaîne de caractères d'entrée et renvoie la nouvelle chaîne de caractères en minuscules. Voici un exemple d'utilisation de cette fonction:

```Haskell
lowerCaseString "Programmation En Haskell"
```
Résultat: "programmation en haskell"

## Plongée en profondeur:
La conversion de chaînes de caractères en minuscules a été populaire depuis les premiers jours de la programmation informatique. Cela a été particulièrement utile pour les langages de programmation qui étaient sensibles à la casse, c'est-à-dire qui différenciaient les majuscules et les minuscules.

Il existe également d'autres façons de réaliser cette tâche en utilisant des outils différents, tels que des expressions régulières ou des fonctions spécifiques de manipulation de chaînes de caractères. Cependant, la méthode avec la fonction map toLower reste simple et efficace.

Pour ceux qui sont intéressés, voici comment la fonction toLower est implémentée dans la bibliothèque standard Haskell:
```Haskell
toLower c
    | fromEnum 'A' <= n && n <= fromEnum 'Z' = toEnum (n - fromEnum 'A' + fromEnum 'a')
    | otherwise = c
    where n = fromEnum c
```
Cette fonction utilise la fonction prédéfinie fromEnum pour convertir un caractère en sa représentation numérique, puis effectue des vérifications pour s'assurer qu'il s'agit d'une lettre majuscule avant de la convertir en minuscule.

## Voir aussi:
- [Documentation officielle de Data.Char](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Char.html)
- [Tutoriel Haskell pour débutants](https://www.fpcomplete.com/haskell/tutorial)