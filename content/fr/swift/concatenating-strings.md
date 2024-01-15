---
title:                "Concaténation de chaînes"
html_title:           "Swift: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes en train d'apprendre Swift, vous avez probablement entendu parler de la concaténation de chaînes de caractères. Cela peut sembler être une compétence de programmation assez basique, mais c'est en fait une compétence très utile qui vous permet de combiner plusieurs chaînes de caractères en une seule.

## Comment le faire

La concaténation de chaînes de caractères en Swift est très simple. Tout ce que vous avez à faire est d'utiliser l'opérateur "+" entre deux chaînes de caractères pour les combiner en une seule. Regardez l'exemple ci-dessous pour mieux comprendre :

```Swift
let nom = "Sophie"
let message = "Bonjour " + nom
print(message)
```

Lorsque vous exécutez ce code, vous verrez que le message imprimé est "Bonjour Sophie". Vous avez réussi à concaténer deux chaînes de caractères en utilisant l'opérateur "+".

## Plongée en profondeur

Maintenant que vous avez compris comment concaténer des chaînes de caractères, voici quelques informations supplémentaires qui pourraient vous être utiles. Premièrement, vous pouvez également utiliser l'opérateur "+" pour concaténer plus de deux chaînes de caractères en les mettant les unes après les autres, par exemple : "Bonjour " + nom + " comment vas-tu ?".

Deuxièmement, si vous avez besoin d'inclure des variables dans votre chaîne de caractères, vous pouvez utiliser l'interpolation de chaîne en plaçant vos variables entre les accolades. Regardons un exemple :

```Swift
let age = 27
let message = "J'ai \(age) ans"
print(message)
```

Dans ce cas, le message imprimé sera "J'ai 27 ans". L'utilisation de l'interpolation de chaîne est très pratique lorsque vous avez besoin de combiner une variable avec une chaîne de caractères.

## Voir aussi

Pour en savoir plus sur la concaténation de chaînes de caractères en Swift, vous pouvez consulter ces ressources :

- [Documentation officielle de Swift sur les chaînes de caractères](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Un tutoriel sur la concaténation de chaînes de caractères en Swift](https://www.hackingwithswift.com/read/22/overview)
- [Stack Overflow : Comment concaténer des chaînes de caractères en Swift](https://stackoverflow.com/questions/24005247/how-do-you-concatenate-strings-in-swift)

Maintenant que vous avez appris à concaténer des chaînes de caractères en Swift, vous pouvez utiliser cette compétence pour créer des messages personnalisés, des noms de fichiers dynamiques ou tout simplement pour rendre votre code plus lisible. Amusez-vous bien !