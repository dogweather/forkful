---
title:                "Elm: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

L'une des choses les plus courantes que nous faisons en tant que développeurs est de manipuler des chaînes de caractères. Nous concaténons des mots pour créer des phrases, des variables pour créer des messages personnalisés, et bien plus encore. Mais pourquoi est-il important de bien comprendre comment concaténer des chaînes en Elm ?

## Comment procéder

Pour concaténer des chaînes en Elm, nous pouvons utiliser l'opérateur `++`. Voyons un exemple simple :

```Elm
let phrase = "Bonjour" ++ " " ++ "le monde"
```

Ici, nous avons créé une variable `phrase` qui contient la chaîne "Bonjour le monde" en utilisant l'opérateur `++` pour concaténer les différentes parties de la phrase.

Nous pouvons également utiliser la fonction `String.concat` pour concaténer plusieurs chaînes en une seule. Voici un exemple :

```Elm
let mots = ["Bonjour", "à", "tous"]
let phrase = String.concat mots
```

Dans cet exemple, la fonction `String.concat` prend en paramètre une liste de chaînes et les concatène toutes ensemble pour créer une nouvelle chaîne.

Lorsque nous exécutons ce code, nous obtenons le résultat suivant :

```
"Bonjour à tous"
```
Cela peut sembler simple, mais savoir comment concaténer correctement des chaînes est essentiel pour de nombreux aspects du développement en Elm.

## Plongée plus profonde

Il existe différentes manières de concaténer des chaînes en Elm, en fonction du contexte et de ce que nous voulons accomplir. Par exemple, nous pouvons également utiliser des chaînes de format pour créer des messages dynamiques. Voici un exemple :

```Elm
let nom = "Marc"
let message = "Bonjour, {nom} !"
```

En utilisant la chaîne de format `{nom}`, nous pouvons créer un message personnalisé en remplaçant `{nom}` par la valeur de la variable `nom`. Cela est très utile lorsque nous voulons créer des messages dynamiques qui changent en fonction des données que nous avons.

Il est également important de noter que, lors de la concaténation de chaînes en Elm, il est important d'utiliser des virgules pour séparer les différentes parties de la chaîne. Cela garantit que tout est correctement concaténé et évite les erreurs.

## Voir aussi

- [Documentation officielle Elm : Manipulation de chaînes](https://guide.elm-lang.org/appendix/strings.html)
- [Démos Elm : Manipulation de chaînes](https://ellie-app.com/new)
- [Tutoriel vidéo : Les chaînes en Elm](https://www.youtube.com/watch?v=xyU8DgziZP4)