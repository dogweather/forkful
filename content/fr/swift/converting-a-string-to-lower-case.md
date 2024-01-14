---
title:                "Swift: Transformer une chaîne de caractères en minuscules"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi
La conversion d'une chaîne de caractères en minuscules peut être utile dans de nombreux cas, notamment pour faciliter la comparaison de chaînes ou pour respecter une convention de nommage dans votre code.

## Comment le faire
La façon la plus simple de convertir une chaîne de caractères en minuscules en Swift est d'utiliser la méthode `lowercased()`. Voici un exemple de code :

```Swift
let string = "BONJOUR"
let lowercasedString = string.lowercased()
print(lowercasedString)
```

Cela produira la sortie suivante : `bonjour`.

Vous pouvez également utiliser la méthode `caseInsensitiveCompare()` pour comparer des chaînes sans tenir compte de la casse. Par exemple :

```Swift
let string1 = "hello"
let string2 = "HELLO"
if string1.caseInsensitiveCompare(string2) == .orderedSame {
    print("Les deux chaînes sont identiques.")
}
```

## Plongée en profondeur
Il est important de noter que la conversion en minuscules dépend de la locale actuelle de l'appareil. Cela signifie que la façon dont les caractères sont accentués peut varier en fonction de la langue et de la région définies sur votre appareil.

De plus, si vous utilisez des caractères Unicode, il est recommandé d'utiliser la méthode `lowercased(with:)` pour prendre en compte les règles de conversion spécifiques à chaque langue. Voici un exemple :

```Swift
let string = "ÉTÉ"
let lowercasedString = string.lowercased(with: Locale(identifier: "fr_FR"))
print(lowercasedString)
```

Cela produira la sortie : `été`.

## Voir aussi
- [Documentation Apple sur la méthode `lowercased()`](https://developer.apple.com/documentation/swift/string/3126929-lowercased)
- [Documentation Apple sur la méthode `caseInsensitiveCompare()`](https://developer.apple.com/documentation/foundation/nsstring/1417566-caseinsensitivecompare)
- [Guide Unicode sur la casse et la normalisation](https://unicode.org/reports/tr21/#Case_Mapping)