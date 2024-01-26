---
title:                "Utilisation des expressions régulières"
html_title:           "Bash: Utilisation des expressions régulières"
simple_title:         "Utilisation des expressions régulières"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Les expressions régulières (regex) permettent de chercher et de manipuler du texte selon des motifs définis. Les développeurs s'en servent pour la validation des données, le scrapping ou encore le traitement de chaînes de caractères complexes.

## Comment faire :
```Swift
import Foundation

let regex = try! NSRegularExpression(pattern: "\\b(\\w+)\\b")
let testString = "Ce sont des exemples simples."
let matches = regex.matches(in: testString, range: NSRange(testString.startIndex..., in: testString))

for match in matches {
    if let range = Range(match.range, in: testString) {
        print(testString[range])
    }
}
```
Sortie :
```
Ce
sont
des
exemples
simples
```

## Plongée Profonde
Historiquement, les regex viennent des sciences théoriques de l'informatique et de la théorie des formalismes. En Swift, on utilise `NSRegularExpression` qui vient d'Objective-C. Comme alternative, on peut utiliser des bibliothèques comme RegexKitLite, mais `NSRegularExpression` est puissant et intégré. L'implantation se base sur ICU, une norme puissante pour les expressions régulières.

## Voir Aussi
- Documentation Apple sur `NSRegularExpression`: [Lien](https://developer.apple.com/documentation/foundation/nsregularexpression)
- Guide ICU pour les expressions régulières : [Lien](https://unicode-org.github.io/icu/userguide/strings/regexp.html)
