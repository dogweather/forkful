---
title:    "Swift: Utilisation des expressions régulières"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# Pourquoi utiliser les expressions régulières en Swift

Si vous êtes un développeur iOS ou macOS, vous avez probablement déjà entendu parler des expressions régulières. Mais pourquoi devriez-vous vous intéresser à eux ? En bref, les expressions régulières sont un moyen puissant de rechercher et de manipuler du texte en utilisant des motifs spécifiques. Cela peut être extrêmement utile pour diverses tâches de traitement de texte, comme la validation d'entrées utilisateur, la recherche de mots clés spécifiques dans un document ou la manipulation de chaînes de caractères complexes.

# Comment utiliser les expressions régulières en Swift

La première étape pour utiliser les expressions régulières en Swift est d'importer la bibliothèque NSRegularExpression. Ensuite, vous pouvez commencer à créer vos propres motifs de recherche en utilisant des caractères spéciaux et des classes de caractères. Par exemple, si vous souhaitez trouver tous les numéros de téléphone dans une chaîne de caractères donnée, vous pouvez utiliser le motif "\d{3}-\d{3}-\d{4}" qui représente le format américain typique. Voici un exemple de code :

```Swift
import Foundation

let text = "Mon numéro de téléphone est 555-555-1234."
let pattern = "\d{3}-\d{3}-\d{4}"
do {
    let regex = try NSRegularExpression(pattern: pattern)
    let results = regex.matches(in: text, range: NSRange(text.startIndex..., in: text))
    for result in results {
        print(text[Range(result.range, in: text)!])
    }
} catch let error {
    print("Invalid pattern: \(error.localizedDescription)")
}

// Output: 555-555-1234
```

Comme vous pouvez le voir dans cet exemple, les expressions régulières en Swift peuvent être utilisées avec la classe NSRegularExpression pour rechercher et extraire des parties spécifiques d'un texte. Vous pouvez également effectuer des substitutions, des validations et bien plus encore en utilisant des expressions régulières.

# Plongez plus loin dans les expressions régulières en Swift

Il y a beaucoup à explorer en termes d'expressions régulières en Swift. Vous pouvez utiliser des modificateurs pour changer la façon dont les recherches sont effectuées, ou utiliser des groupes de capture pour extraire des parties spécifiques d'une correspondance. De plus, il existe de nombreux sites et applications qui peuvent vous aider à tester et à perfectionner vos expressions régulières. N'hésitez pas à expérimenter et à pratiquer pour maîtriser cette puissante fonctionnalité de Swift.

# Voir aussi

- Documentation Apple sur les expressions régulières en Swift : https://developer.apple.com/documentation/foundation/nsregularexpression
- Un outil en ligne pour tester et expérimenter avec les expressions régulières : https://regex101.com/
- Un livre sur l'utilisation des expressions régulières en Swift : https://www.amazon.com/dp/B0917B8ZVB