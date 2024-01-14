---
title:    "Swift: Recherche et remplacement de texte"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Pourquoi

La recherche et le remplacement de texte sont des tâches courantes en programmation, souvent utilisées pour corriger des erreurs ou pour apporter des modifications à grande échelle dans un code existant. En utilisant quelques techniques simples, vous pouvez automatiser ces tâches et économiser du temps et des efforts.

## Comment faire

Pour rechercher et remplacer du texte en utilisant le langage Swift, vous devez utiliser la méthode `replacingOccurrences(of:with:)` sur votre chaîne de caractères. Par exemple, si vous souhaitez remplacer toutes les occurrences du mot "hello" par "bonjour" dans votre chaîne, vous pouvez utiliser le code suivant :

```Swift
let text = "hello world hello"
let newText = text.replacingOccurrences(of: "hello", with: "bonjour")
print(newText)
```

Cela produira la sortie suivante :

```Swift
bonjour world bonjour
```

Vous pouvez également utiliser cette méthode avec d'autres types de remplacement, tels que "character set" ou "regular expression", en utilisant les options disponibles dans la méthode `replacingOccurrences`.

## Plongez plus profondément

Outre la méthode `replacingOccurrences`, Swift offre d'autres outils pour la recherche et le remplacement de texte, tels que les fonctions `range(of:options:range:locale:)` et `replaceAll(matching:with:)`. Vous pouvez également utiliser des librairies externes telles que Regex pour étendre les fonctionnalités de recherche et de remplacement.

Assurez-vous de bien comprendre les différentes options disponibles et comment elles influencent les résultats lors de la recherche et du remplacement de texte.

## Voir aussi

- [Documentation officielle Apple](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID301) sur la manipulation de chaînes de caractères en Swift
- [Article sur Hacking with Swift](https://www.hackingwithswift.com/example-code/strings/how-to-use-regular-expressions-in-swift) sur l'utilisation des expressions régulières en Swift
- [Librairie Regex](https://github.com/sharplet/Regex) pour la manipulation avancée de texte en Swift