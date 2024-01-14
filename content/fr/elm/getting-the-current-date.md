---
title:                "Elm: Obtenir la date actuelle"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Pourquoi obtenir la date actuelle en Elm
La programmation en Elm est de plus en plus populaire en raison de son langage simple et fonctionnel, ainsi que de sa fiabilité. Il est souvent nécessaire d'obtenir la date actuelle dans les applications web pour afficher l'heure et la date, ou pour gérer des tâches en temps réel. Dans cet article, nous allons explorer comment obtenir la date actuelle en Elm.

## Comment faire
Pour obtenir la date actuelle en Elm, nous allons utiliser la fonction `Time.now` de la bibliothèque de base de Elm. Cette fonction renvoie un `Time` qui représente la date et l'heure actuelles en utilisant le fuseau horaire UTC. Nous pouvons ensuite transformer cette valeur `Time` en une chaîne de caractères avec la fonction `Time.toString`.

```Elm
import Time exposing (..)

currentDate : String
currentDate =
    let
        time = Time.now
    in
        Time.toString "%d/%m/%Y" time -- Format de date personnalisé
```

La sortie de ce code sera une chaîne de caractères au format jour/mois/année, par exemple "17/08/2021".

## Plongée profonde
Il est important de comprendre que la fonction `Time.now` est basée sur l'heure système de l'ordinateur de l'utilisateur. Cela signifie que si l'heure de l'ordinateur est incorrecte, la date renvoyée par la fonction `Time.now` sera également incorrecte. Il est donc essentiel de vérifier et d'ajuster l'heure système avant d'utiliser cette fonction.

Il est également possible de créer un horodateur en temps réel en utilisant la valeur `Time` renvoyée par `Time.now`. En utilisant des abonnements et la fonctionnalité de mise à jour en temps réel d'Elm, nous pouvons mettre à jour l'affichage de la date et de l'heure à chaque seconde. Cela peut être utile pour les applications de surveillance en temps réel ou pour créer des minuteries.

# Voir aussi
- [Documentation de la fonction Time.now dans la bibliothèque de base de Elm](https://package.elm-lang.org/packages/elm/time/latest/Time#now)
- [Guide de la mise à jour en temps réel dans Elm](https://guide.elm-lang.org/effects/time.html)