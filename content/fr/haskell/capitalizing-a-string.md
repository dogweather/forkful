---
title:                "Haskell: Capitalisation d'une chaîne de caractères"
simple_title:         "Capitalisation d'une chaîne de caractères"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi 

Si vous êtes un développeur Haskell en herbe, vous avez peut-être entendu parler de la fonction `toUpper` et vous vous êtes demandé à quoi elle servait. Eh bien, dans ce billet, nous allons vous montrer pourquoi et comment capitaliser une chaîne de caractères en utilisant Haskell. 

## Comment faire 

Pour commencer, nous allons définir une fonction `capitalize` qui prend une chaîne de caractères en entrée et renvoie une version capitalisée de cette chaîne. 

```Haskell 
capitalize :: String -> String 
capitalize str = map toUpper str 
``` 

Et voilà ! En utilisant la fonction intégrée `toUpper` de Haskell, nous pouvons appliquer la transformation souhaitée à notre chaîne de caractères. Voyons cela en action avec un exemple concret : 

```Haskell 
capitalize "bonjour" 
-- "BONJOUR" 
``` 

Nous pouvons également appliquer cette fonction à des chaînes de caractères plus longues : 

```Haskell 
capitalize "ceci est une phrase" 
-- "CECI EST UNE PHRASE" 
``` 

## Plongez plus profondément 

Maintenant que vous savez comment capitaliser une chaîne de caractères en utilisant Haskell, vous vous demandez peut-être comment cela fonctionne réellement. En fait, la fonction `toUpper` parcourt simplement la chaîne de caractères et remplace chaque lettre minuscule par sa version majuscule correspondante en utilisant la table Unicode. Elle ignore également les caractères qui ne peuvent pas être convertis en majuscules. 

## Voir aussi 

- Documentation officielle de `toUpper` sur le site de Haskell : https://www.haskell.org/onlinereport/standard-prelude.html#idd1 Sanitize https://en.wikipedia.org/wiki/Haskell
- Tutoriel sur les fonctions intégrées de Haskell : https://www.tutorialspoint.com/haskell/haskell_characters.htm 
- Vidéo de présentation de Haskell sur YouTube : https://www.youtube.com/watch?v=02_H3LjqMr8
- Exercices pratiques pour améliorer vos compétences en Haskell : https://www.codewars.com/kata/search/haskell?q=&tags=Haskell