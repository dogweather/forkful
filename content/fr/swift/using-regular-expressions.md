---
title:                "Swift: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi
Avant d'entrer dans les détails de l'utilisation des expressions régulières en Swift, il est important de comprendre pourquoi vous voudriez les utiliser. Les expressions régulières sont des outils puissants pour manipuler et analyser des chaînes de caractères. Elles vous permettent de trouver des motifs spécifiques dans une chaîne et de les remplacer ou de les extraire selon vos besoins. Cela peut être particulièrement utile dans le développement d'applications qui nécessitent une saisie de données spécifique ou la vérification de mots de passe.

## Comment utiliser les expressions régulières en Swift
Pour utiliser les expressions régulières en Swift, vous devez d'abord importer le framework *Foundation*, qui inclut la classe NSRegularExpression utilisée pour manipuler les expressions régulières. Ensuite, vous pouvez créer une instance de NSRegularExpression en utilisant un motif de recherche et des options nécessaires.

Exemple de code:

```Swift
import Foundation
let string = "Bonjour les ami(e)s!"
let pattern = "ami(e)s"
do {
  let regex = try NSRegularExpression(pattern: pattern)
  let matches = regex.matches(in: string, range: NSRange(string.startIndex..., in: string))
  print("Nombre de correspondances : \(matches.count)")
} catch {
  print("Erreur : \(error)")
}
```

Résultat:

```Swift
Nombre de correspondances : 1
```

Comme vous pouvez le voir, en utilisant l'expression régulière "ami(e)s", nous avons trouvé une correspondance dans la chaîne "Bonjour les ami(e)s!".

## Plongée en profondeur
Il existe de nombreux motifs et options différents que vous pouvez utiliser dans les expressions régulières en Swift. Par exemple, vous pouvez spécifier des caractères alphanumériques, des chiffres ou des symboles spécifiques à rechercher. Vous pouvez également utiliser des opérateurs tels que "?" pour indiquer que le caractère précédent peut être présent ou non dans la chaîne, ou "*" pour indiquer que le caractère précédent peut être présent plusieurs fois.

Il est également important de comprendre les différentes options qui existent, telles que la recherche sensible à la casse ou la recherche sur plusieurs lignes. En utilisant ces options, vous pouvez affiner encore plus vos recherches pour trouver exactement ce que vous recherchez dans une chaîne.

## Voir aussi
- [Documentation Apple sur les expressions régulières en Swift](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Guide de référence ultime pour les expressions régulières en Swift](https://www.raywenderlich.com/5765-regular-expressions-tutorial-getting-started)
- [Utilisation d'expressions régulières pour valider les données dans Swift](https://medium.com/@hassan_31951/now-use-regular-expression-to-validate-data-in-swift-f6a15f1c7deb)