---
title:                "Swift: Rechercher et remplacer du texte"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Swift, vous savez probablement que le processus de recherche et de remplacement de texte est une partie inévitable de notre travail. Que vous soyez en train de corriger une simple faute de frappe ou de modifier des milliers de lignes de code, savoir comment faire efficacement une recherche et un remplacement peut vous faire gagner beaucoup de temps et vous éviter bien des maux de tête.

## Comment faire

Voici quelques exemples de code en Swift pour vous montrer comment vous pouvez effectuer une recherche et un remplacement de texte dans votre code :

```
// Recherche et remplacement d'un certain mot dans une chaîne

let originalString = "Bonjour le monde"
let remplacéString = originalString.replacingOccurrences(of: "Bonjour", with: "Salut")

print(remplacéString) // Sortie : Salut le monde


// Remplacement de lettres spécifiques dans une chaîne

let animal = "chat"

if let replacedAnimal = animal.replacingOccurrences(of: "a", with: "e") {
    print(replacedAnimal) // Sortie : chet
}

```

Ces exemples montrent comment vous pouvez facilement remplacer du texte dans une chaîne en utilisant la méthode `replacingOccurences(of:)`. Cette méthode prend deux paramètres : le premier est le texte que vous souhaitez remplacer, et le deuxième est le texte avec lequel vous souhaitez le remplacer.

Pour rechercher un texte spécifique, vous pouvez utiliser la méthode `range(of:)`, qui renvoie un `Range<String.Index>` contenant l'emplacement du texte recherché. Ensuite, vous pouvez utiliser cette information pour effectuer un remplacement si nécessaire.

## Deep Dive

Maintenant que vous savez comment faire une recherche et un remplacement de base dans Swift, il est important de comprendre comment cela fonctionne en profondeur. Tout d'abord, il est important de noter que les méthodes de recherche et de remplacement de texte en Swift sont sensibles à la casse. Cela signifie que "Hello" et "hello" seront considérés comme deux chaînes différentes. Si vous souhaitez effectuer une recherche et un remplacement sans tenir compte de la casse, vous pouvez utiliser la méthode `range(of:, options:)` et spécifier l'option `.caseInsensitive`.

De plus, il est important de comprendre la différence entre les chaînes et leurs index en Swift. Les chaînes en Swift sont en fait des collections de caractères, et chaque caractère a un index qui correspond à sa position dans la chaîne. Les index d'une chaîne Swift sont de type `String.Index`, et ils peuvent être utilisés pour accéder à des caractères individuels ou pour déterminer l'emplacement d'un certain texte dans la chaîne.

## Voir aussi

- [Documentation officielle sur les méthodes de recherche et de remplacement de chaîne en Swift](https://developer.apple.com/documentation/foundation/nsstring/1411943-replacingoccurrences)
- [Article sur l'utilisation des options de recherche et de remplacement de chaîne en Swift](https://www.hackingwithswift.com/example-code/strings/how-to-use-regular-expressions-to-find-and-replace-text)
- [Guide complet sur les chaînes en Swift](https://www.swiftbysundell.com/posts/strings-in-swift-4)