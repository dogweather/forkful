---
title:                "Haskell: Trouver la longueur d'une chaîne de caractères"
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi 

La recherche de la longueur d'une chaîne de caractères est une tâche courante dans la programmation Haskell. Savoir comment le faire peut améliorer l'efficacité de votre code et vous permettre de résoudre des problèmes plus rapidement. Dans cet article, nous allons explorer différentes façons de trouver la longueur d'une chaîne en Haskell.

## Comment faire

Il existe plusieurs façons de trouver la longueur d'une chaîne en Haskell. Nous allons en explorer deux qui sont couramment utilisées : la fonction `length` et une approche récursive.

La fonction `length` est intégrée à Haskell et peut être utilisée de la manière suivante :

```Haskell
length :: [a] -> Int
```

Cette fonction prend une liste en entrée et renvoie un entier représentant la longueur de la liste. Par exemple, pour trouver la longueur de la chaîne de caractères "Bonjour", on peut utiliser la fonction de cette manière :

```Haskell
length "Bonjour" 
```
Ce qui renverra 7 caractères.

Une autre méthode pour trouver la longueur d'une chaîne est d'utiliser une approche récursive. Voici un exemple de fonction récursive qui renvoie la longueur d'une chaîne :

```Haskell
strlength :: [a] -> Int
    strlength [] = 0
    strlength (x:xs) = 1 + strlength xs
```

Cette fonction prend une liste de caractères en entrée et utilise une méthode récursive pour ajouter 1 à chaque caractère jusqu'à ce que la liste soit vide, renvoyant ainsi la longueur totale de la chaîne.

## Deep Dive

Si vous souhaitez en savoir plus sur la fonction `length` en Haskell, vous pouvez consulter la documentation officielle de la fonction sur le site web de Haskell. Vous y trouverez des informations sur son implémentation et son efficacité.

Il est également intéressant de noter que la fonction `length` peut être utilisée pour trouver la longueur de toutes sortes de structures de données en Haskell, pas seulement les chaînes de caractères. Elle peut être utilisée pour les listes, les tableaux, les arbres, etc.

## Voir aussi

Pour en savoir plus sur les fonctions intégrées en Haskell, vous pouvez consulter les liens suivants :

- https://www.haskell.org/documentation/ : Documentation officielle de Haskell
- https://wiki.haskell.org/Haskell : Wiki officiel de Haskell
- https://www.haskell.org/hoogle/ : Moteur de recherche Haskell pour trouver des fonctions intégrées et des documents

J'espère que cet article vous a été utile pour comprendre comment trouver la longueur d'une chaîne en Haskell. N'hésitez pas à explorer d'autres méthodes et à les expérimenter dans votre code pour trouver celle qui répond le mieux à vos besoins. Bonne programmation !