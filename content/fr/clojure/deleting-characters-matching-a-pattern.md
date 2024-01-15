---
title:                "Suppression de caractères correspondant à un motif"
html_title:           "Clojure: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

On a tous été confrontés à des situations où l'on doit supprimer des caractères dans une chaîne de texte qui correspondent à certaines conditions. Peut-être que vous voulez nettoyer une liste de noms en retirant les numéros de téléphone associés. Ou peut-être que vous voulez extraire une partie spécifique d'une chaîne pour l'utiliser ailleurs. Dans cet article, je vais vous montrer comment supprimer des caractères en utilisant la puissance de Clojure.

## Comment Faire

Il existe plusieurs façons de supprimer des caractères dans une chaîne de texte en utilisant Clojure. L'une des méthodes les plus simples consiste à utiliser la fonction `replace` et à fournir une expression régulière en tant que motif. Par exemple, supposons que nous voulions supprimer tous les chiffres d'une chaîne de texte :

```Clojure
(def texte "Il était une fois 1, 2, 3")
(replace texte #"[0-9]" "")
```
Ce code renverra la chaîne de texte "Il était une fois , , ".

Si vous voulez supprimer des caractères en fonction de leur position dans la chaîne, vous pouvez utiliser la fonction `substring`. Par exemple, supposons que nous voulions supprimer les trois premiers caractères d'une chaîne :

```Clojure
(def texte "abcdef")
(substring texte 3)
```
Cela renverra "def".

Il existe également d'autres fonctions utiles comme `drop-while` et `take-while` qui permettent de supprimer des caractères jusqu'à ce qu'une condition soit remplie. Par exemple, si vous voulez supprimer des caractères jusqu'à ce que vous trouviez un espace, vous pouvez utiliser `drop-while` de cette manière :

```Clojure
(def texte "Bonjour tout le monde")
(drop-while #(not= \space %) texte)
```
Cela renverra la chaîne de texte " tout le monde".

## Plongée plus profonde

Lorsque vous supprimez des caractères, il est important de comprendre comment Clojure gère les chaînes de texte. En interne, Clojure utilise une structure de données appelée Persistent Vector pour stocker les chaînes. Cela signifie que chaque fois que vous modifiez une chaîne, une nouvelle copie de la chaîne est créée. Cela peut entraîner une surcharge de la mémoire si vous travaillez avec de très grandes chaînes ou si vous effectuez de nombreuses modifications.

Un moyen de contourner ce problème consiste à utiliser `transients`, qui font partie de l'API de base de Clojure. Les `transients` vous permettent de modifier une chaîne de manière mutable, évitant ainsi la surcharge de la mémoire. Vous pouvez ensuite utiliser la fonction `persistent!` pour convertir le résultat en une chaîne immuable.

De plus, si vous travaillez avec des chaînes multilignes ou avec des caractères unicode, il est important de prendre en compte certaines subtilités de traitement dans Clojure. Vous devrez peut-être utiliser des expressions régulières spécifiques ou utiliser les fonctions `clip` ou `drop-last` pour gérer les retours à la ligne et autres caractères spéciaux.

## Voir Aussi

- [Documentation officielle de Clojure](https://clojure.org/)
- [Le livre "Clojure for the Brave and True" de Daniel Higginbotham](https://www.braveclojure.com/clojure-for-the-brave-and-true/)
- [La chaîne YouTube "ClojureTV", animée par Bozhidar Batsov](https://www.youtube.com/channel/UCeC6jJIFZImzH9OGjnL8r5Q)