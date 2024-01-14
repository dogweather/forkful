---
title:                "Swift: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur Swift, vous savez à quel point il est important de maintenir un code propre et organisé. Parfois, cela signifie devoir faire des recherches et des remplacements de texte dans votre code. Dans cet article, nous allons explorer comment faire cela en utilisant Swift.

## Comment faire

La première chose à faire pour rechercher et remplacer du texte est d'utiliser la méthode `replacingOccurrences(of:with:)` sur votre chaîne de caractères. Voici un exemple de code:

```Swift
var string = "Bonjour le monde !"
string = string.replacingOccurrences(of: "Bonjour", with: "Salut")
print(string)
```

La sortie de ce code sera `Salut le monde !`, car nous avons remplacé le mot "Bonjour" par "Salut". Vous pouvez également spécifier un nombre maximum de remplacements grâce à un troisième paramètre optionnel dans la méthode.

Outre la méthode `replacingOccurrences`, vous pouvez également utiliser la méthode `replacingOccurrences(of:with:options:range:)` pour avoir plus de contrôle sur la recherche et le remplacement. Cette méthode vous permet de spécifier des options telles que l'ignorance ou non de la casse, ainsi que de limiter la recherche à une certaine plage de caractères.

## Deep Dive

Lorsque vous effectuez un remplacement de texte en utilisant Swift, il est important de comprendre que cela se fait de manière non destructive, ce qui signifie que la chaîne de caractères d'origine ne sera pas modifiée. Au lieu de cela, un nouveau résultat sera créé et renvoyé.

Vous pouvez également utiliser des expressions régulières pour rechercher et remplacer du texte en utilisant la bibliothèque `Foundation`. Il vous suffit de créer une expression régulière en utilisant la classe `NSRegularExpression`, puis de l'utiliser dans la méthode `replacingOccurrences(of:with:options:range:)`.

## Voir aussi

- Apple Swift documentation officielle pour `replacingOccurrences`: https://developer.apple.com/documentation/foundation/nsstring/1411943-replacingoccurrences
- Tutoriel Apprenez Swift: Remplacer et modifier du texte: https://www.apprendre-swift.com/remplacer-modifier-texte/
- Site Web officiel de Swift: https://swift.org/