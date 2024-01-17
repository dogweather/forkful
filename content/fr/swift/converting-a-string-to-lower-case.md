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

## Qu'est-ce que c'est et pourquoi?
Convertis une chaîne de caractères en minuscules signifie simplement changer toutes les lettres majuscules d'une chaîne en lettres minuscules. Les programmeurs font cela pour rendre les données plus cohérentes et faciles à manipuler.

## Comment:
Voici quelques exemples de code pour convertir une chaîne de caractères en minuscules en Swift:

```Swift
let mot = "Bonjour"
let motEnMinuscules = mot.lowercased()
print(motEnMinuscules)
```

Cela produira la sortie suivante:

```
bonjour
```

Vous pouvez également utiliser la fonction `map` pour appliquer la conversion à chaque caractère de la chaîne:

```Swift
let phrase = "Bonjour à tous!"
let phraseEnMinuscules = phrase.map { $0.lowercased() }.joined()
print(phraseEnMinuscules)
```

Cela produira la sortie suivante:

```
bonjour à tous!
```

## Plongée en profondeur:
Avant Swift, la méthode courante pour convertir une chaîne de caractères en minuscules était d'utiliser la fonction `lowercaseString` disponible dans la classe `NSString` de Foundation. Cependant, cette méthode était lente et ne fonctionnait qu'avec des caractères ASCII. Avec Swift, la fonction `lowercased` est plus rapide et prend en charge tous les caractères de l'unicode.

Il existe également d'autres méthodes pour convertir une chaîne en minuscules, telles que l'utilisation de l'opérateur d'affectation étendue (`string.lowercased()`) ou l'utilisation de la structure `CharacterSet` pour déterminer les caractères à convertir.

## Voir aussi:
- [La documentation officielle Swift sur la fonction `lowercased`](https://developer.apple.com/documentation/swift/string/2894704-lowercased)
- [Une comparaison de performances entre les différentes méthodes de conversion en minuscules en Swift](https://www.hackingwithswift.com/articles/174/the-performance-of-strings-in-swift)
- [La documentation officielle sur la structure `CharacterSet`](https://developer.apple.com/documentation/foundation/characterset)