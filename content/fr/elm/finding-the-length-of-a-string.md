---
title:                "Elm: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Pourquoi

Si vous êtes un programmeur en herbe ou un développeur expérimenté à la recherche d'un nouveau langage de programmation fonctionnel, vous avez peut-être entendu parler d'Elm. Ce langage de programmation fonctionnelle pur est en train de gagner en popularité grâce à sa simplicité, sa stabilité et sa performance. L'une des tâches les plus couramment effectuées en programmation est la recherche de la longueur d'une chaîne de caractères. Dans cet article, nous allons explorer comment trouver la longueur d'une chaîne de caractères en utilisant Elm.

# Comment faire

Tout d'abord, nous allons définir une chaîne de caractères en utilisant la fonction ```String.fromString```. Ensuite, nous pouvons utiliser la fonction ```String.length``` pour trouver la longueur de la chaîne. Voici un exemple de code pour mieux comprendre :

```
import String exposing (fromString, length)

-- Définition de la chaîne de caractères
let
    texte = fromString "Bonjour le monde!"
in
    -- Utilisation de la fonction String.length pour trouver la longueur
    textLength = length texte
```
Lorsque nous exécutons cette fonction, la valeur de ```textLength``` sera de 18, car il y a 18 caractères dans la chaîne de caractères "Bonjour le monde!".

# Profondeur

Maintenant que nous savons comment trouver la longueur d'une chaîne de caractères en Elm, plongeons un peu plus profondément dans le fonctionnement de la fonction ```String.length```. Tout d'abord, il est important de noter que cette fonction renvoie un entier représentant le nombre de caractères dans la chaîne donnée. Il est également important de comprendre que les caractères unicode comptent comme un seul caractère.

De plus, si la chaîne de caractères contient des éléments tels que des espaces, ils seront également pris en compte dans la longueur totale. Par exemple, si nous avons une chaîne de caractères "Salut à tous", la longueur sera de 11, car il y a un espace entre les mots.

# Voir aussi
- [Documentation officielle Elm](https://guide.elm-lang.org/)
- [Tutoriels d'apprentissage Elm](https://blog.johanr.com/learning-elm)
- [Communauté Elm France](https://www.facebook.com/groups/elmfr/)
- [Chaînes de caractères en Elm](https://programmation.quebec/elm/getting-string-length)

Merci d'avoir lu cet article sur la recherche de la longueur d'une chaîne de caractères en Elm. Espérons que cela vous a donné un aperçu de la simplicité et de la puissance de ce langage de programmation fonctionnelle. N'hésitez pas à partager vos commentaires et vos expériences avec Elm dans la section des commentaires ci-dessous. Bonne programmation!