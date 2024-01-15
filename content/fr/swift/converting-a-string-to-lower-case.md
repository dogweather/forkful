---
title:                "Convertir une chaîne en minuscules"
html_title:           "Swift: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous travaillez avec des chaînes de caractères en Swift, il peut être utile de les convertir en minuscules pour différents besoins, tels que la comparaison de chaînes ou la manipulation de données. Dans cet article, nous allons découvrir comment convertir une chaîne de caractères en minuscules en utilisant Swift.

## Comment Faire

Pour convertir une chaîne de caractères en minuscules en Swift, nous utiliserons la méthode `lowercased()` qui est disponible sur n'importe quelle instance de chaîne de caractères. Voyons un exemple de code:

```Swift
let food = "HAMBURGER"
let lowercaseFood = food.lowercased()
print(lowercaseFood)
```
L'output de ce code sera "hamburger", avec toutes les lettres en minuscules. Nous pouvons également directement utiliser la méthode `lowercased()` sur une chaîne de caractères littérale:

```Swift
let drink = "SODA"
print(drink.lowercased())
```
L'output de ce code sera également "soda". La méthode `lowercased()` renvoie une nouvelle chaîne de caractères avec toutes les lettres en minuscules, sans affecter la chaîne originale.

## Plongeon Profond

La méthode `lowercased()` utilise les règles de casse spécifiées par l'algorithme Unicode. Cela signifie que tout caractère qui a une casse en Unicode sera converti en minuscules, y compris les lettres avec des accents et les caractères spéciaux. Cependant, la conversion en minuscules peut varier selon la langue utilisée, car certaines langues ont des règles spécifiques pour la casse. Par exemple, en turc, la conversion en minuscules d'une lettre i majuscule produit une lettre ı minuscule, tandis qu'en anglais, elle produit une lettre i minuscule.

Il est également important de noter que la méthode `lowercased()` ne prend pas en compte la localisation de l'utilisateur. Si votre application prend en charge plusieurs langues, il peut être nécessaire d'utiliser des méthodes alternatives pour convertir une chaîne de caractères en minuscules en fonction de la langue de l'utilisateur.

## Voir Aussi

- [Documentation officielle sur les chaînes de caractères en Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Guide complet sur la manipulation des chaînes de caractères en Swift](https://www.hackingwithswift.com/articles/113/how-to-build-help-nudges-into-your-app-with-swiftstring-extensions)