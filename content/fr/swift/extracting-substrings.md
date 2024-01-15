---
title:                "Extraction de sous-chaînes"
html_title:           "Swift: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous programmez en Swift, vous avez sûrement déjà utilisé des chaînes de caractères. Mais saviez-vous qu'il est possible d'extraire des sous-chaînes d'une chaîne principale ? Dans cet article, nous allons découvrir comment et pourquoi utiliser cette fonctionnalité utile et pratique.

## Comment faire

L'extraction de sous-chaînes en Swift se fait grâce à la méthode `substring` qui prend deux paramètres : un index de début et un index de fin. Voici un exemple de code qui utilise cette méthode :

```Swift
let str = "Bonjour tout le monde"
let subStr = str.substring(from: 8, to: 12)
print(subStr) // affiche "tout"
```

Dans cet exemple, nous avons extrait la sous-chaîne "tout" à partir de la chaîne principale "Bonjour tout le monde" en utilisant les index 8 et 12 qui représentent respectivement le début et la fin de la sous-chaîne souhaitée.

Il est également possible d'utiliser des index négatifs pour commencer à partir de la fin de la chaîne. Par exemple, un index de -9 correspond au caractère situé 9 positions avant la fin de la chaîne.

## Plongeons plus profondément

En plus de l'extraction d'une sous-chaîne à l'aide de la méthode `substring`, il existe également d'autres moyens plus avancés pour récupérer des sous-chaînes en Swift.

- Vous pouvez utiliser la propriété `prefix` pour extraire les premiers caractères d'une chaîne.

- La propriété `suffix` peut être utilisée pour extraire les derniers caractères d'une chaîne.

- La méthode `split` vous permet de séparer une chaîne en différentes sous-chaînes en utilisant un caractère séparateur.

Pour en savoir plus sur ces différentes méthodes et propriétés, n'hésitez pas à consulter la documentation officielle de Swift.

## Voir aussi

- [La documentation officielle de Swift sur les chaînes](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Un tutoriel sur l'extraction de sous-chaînes en Swift](https://www.hackingwithswift.com/example-code/strings/how-to-extract-a-substring-from-a-string)
- [Un article sur la manipulation de chaînes avec Swift](https://www.swiftbysundell.com/basics/strings/)