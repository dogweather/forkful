---
title:                "Swift: Extraction de sous-chaînes"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

L'extraction de sous-chaînes est une compétence importante à avoir en programmation Swift. Elle permet de manipuler et d'extraire des parties spécifiques d'une chaîne de caractères. Cela peut s'avérer utile dans de nombreuses situations, telles que la validation des entrées utilisateur, la recherche de mots-clés dans un texte ou encore la manipulation de données provenant d'une API.

## Comment faire

Pour extraire une sous-chaîne d'une chaîne de caractères en Swift, utilisez la méthode `substring(from:)` ou `prefix(through:)` en fonction de vos besoins. Voici un exemple de code avec l'utilisation de la méthode `substring(from:)` :

```Swift
let phrase = "Je suis un développeur Swift."
let extrait = phrase.substring(from: 11)
print(extrait)  // "développeur Swift."
```

Vous pouvez également utiliser la méthode `range(of:)` pour rechercher un mot-clé ou une expression dans une chaîne de caractères et extraire la sous-chaîne correspondante :

```Swift
let phrase = "Je suis un développeur Swift."
if let range = phrase.range(of: "développeur") {
    let extrait = phrase.substring(with: range)
    print(extrait)  // "développeur"
}
```

## Plongée en profondeur

L'extraction de sous-chaînes peut sembler simple, mais il y a quelques aspects à prendre en compte. Par exemple, la méthode `range(of:)` peut renvoyer `nil` si le mot-clé recherché n'est pas présent dans la chaîne de caractères, il est donc important de prendre en compte cette possibilité lors de la manipulation des sous-chaînes.

Il est également important de comprendre que la méthode `substring(from:)` inclut le caractère à la position indiquée, tandis que la méthode `prefix(through:)` inclut tous les caractères jusqu'à la position indiquée.

## Voir aussi

- [Documentation officielle Apple sur les chaînes de caractères en Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Exemples d'utilisation de la méthode `substring(from:)` en Swift](https://www.hackingwithswift.com/example-code/strings/how-to-extract-a-substring-from-a-string)
- [Guide complet sur la manipulation des sous-chaînes en Swift](https://www.ralfebert.de/ios-examples/uikit/strings/substring/)