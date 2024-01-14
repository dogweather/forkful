---
title:                "Swift: Capitaliser une chaîne de caractères"
simple_title:         "Capitaliser une chaîne de caractères"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

### Pourquoi

La capitalisation de chaînes de caractères est une étape importante dans la programmation en Swift, car elle permet de convertir une chaîne en une autre avec une majuscule au début de chaque mot. Cela peut être utile pour l'affichage de noms propres ou pour respecter les conventions de présentation dans une application.

### Comment faire

Il existe plusieurs façons de capitaliser une chaîne en Swift. Voici deux exemples à l'aide de la fonction `capitalized` :

```
Swift let str = "voiture apple" print(str.capitalized)
// Output: Voiture Apple
```
```
Swift let str = "123abc" print(str.capitalized)
// Output: 123abc (car il n'y a pas de mots à capitaliser)
```

Vous pouvez également utiliser la fonction `capitalized(with: Locale)` pour spécifier une langue spécifique pour la capitalisation. Par exemple, si vous voulez que la première lettre de chaque mot soit en majuscule en français, vous pouvez utiliser la locale `fr_FR` :

```
Swift let str = "pomme orange" print(str.capitalized(with: Locale(identifier: "fr_FR")))
// Output: Pomme Orange
```

### Plongée en profondeur

Lorsque vous utilisez la fonction `capitalized`, il est important de noter que seules les lettres sont converties en majuscules, tandis que les nombres et les symboles sont ignorés. De plus, les majuscules déjà présentes dans la chaîne seront conservées, à moins que vous n'utilisiez la fonction `lowercased` pour tout convertir en minuscules avant de capitaliser.

Une autre chose à considérer est que la fonction `capitalized` ne prend pas en compte les caractères spéciaux tels que les accents ou les cédilles. Pour être plus précis, vous pouvez utiliser la fonction `capitalized(with: CapitalizationOptions)` pour spécifier le type de capitalisation que vous souhaitez. Par exemple, vous pouvez utiliser `.capitalized(exceptFirstCharacter: Bool)` pour capitaliser tous les caractères sauf le premier.

### Voir aussi

- [Documentation officielle de Swift sur la capitalisation de chaînes](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID295)
- [Tutoriel sur la capitalisation de chaînes en Swift](https://www.hackingwithswift.com/example-code/strings/how-to-capitalize-the-first-letter-of-a-string)
- [Exemples de fonction `capitalized` en Swift](https://cocoacasts.com/capitalizing-strings-with-swift)