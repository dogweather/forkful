---
aliases:
- /fr/swift/using-regular-expressions/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:22.109846-07:00
description: "Les expressions r\xE9guli\xE8res, ou regex, sont des s\xE9quences de\
  \ caract\xE8res qui forment un motif de recherche, souvent utilis\xE9 pour des t\xE2\
  ches de\u2026"
lastmod: 2024-02-18 23:09:09.197707
model: gpt-4-0125-preview
summary: "Les expressions r\xE9guli\xE8res, ou regex, sont des s\xE9quences de caract\xE8\
  res qui forment un motif de recherche, souvent utilis\xE9 pour des t\xE2ches de\u2026"
title: "Utilisation des expressions r\xE9guli\xE8res"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Les expressions régulières, ou regex, sont des séquences de caractères qui forment un motif de recherche, souvent utilisé pour des tâches de correspondance ou de manipulation de chaînes. Les programmeurs les utilisent pour tout, de la validation de données et l'analyse au transformations, les rendant un outil indispensable dans les tâches de traitement et de manipulation du texte à travers divers langages de programmation, y compris Swift.

## Comment faire :
La prise en charge native de Swift pour les regex utilise la classe `NSRegularExpression`, ainsi que les méthodes de plage et de remplacement de la classe String. Voici un exemple d'utilisation des regex pour trouver et mettre en évidence des adresses e-mail dans un bloc de texte :

```swift
import Foundation

let text = "Contactez-nous à support@example.com ou feedback@example.org pour plus d'informations."
let regexPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

do {
    let regex = try NSRegularExpression(pattern: regexPattern)
    let matches = regex.matches(dans: text, plage: NSRange(text.startIndex..., dans: text))

    if !matches.isEmpty {
        for match in matches {
            let range = Range(match.range, dans: text)!
            print("Trouvé : \(text[range])")
        }
    } else {
        print("Aucune correspondance trouvée.")
    }
} catch {
    print("Erreur de regex : \(error.localizedDescription)")
}

// Sortie d'exemple :
// Trouvé : support@example.com
// Trouvé : feedback@example.org
```

Pour des scénarios plus complexes ou axés sur la commodité, vous pouvez utiliser des bibliothèques tierces telles que SwiftRegex, qui simplifie la syntaxe et étend les possibilités. Bien que la bibliothèque standard de Swift soit puissante, certains développeurs favorisent ces bibliothèques pour leur syntaxe concise et leurs fonctionnalités supplémentaires. Voici comment vous pourriez effectuer une tâche similaire en utilisant une bibliothèque tierce hypothétique :

```swift
// En supposant qu'une bibliothèque appelée SwiftRegex existe et est importée
let text = "Contactez à hello@world.com ou visitez notre site Web."
let emailPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

let emails = text.matches(pour: emailPattern) // Méthode hypothétique fournie par SwiftRegex
if emails.isEmpty {
    print("Aucune adresse e-mail trouvée.")
} else {
    emails.forEach { email in
        print("Trouvé : \(email)")
    }
}

// Sortie hypothétique en supposant que la méthode `matches(pour:)` existe dans SwiftRegex :
// Trouvé : hello@world.com
```

Cet exemple illustre l'utilisation d'un package d'expression régulière tiers pour simplifier la recherche de correspondances dans une chaîne, en supposant que des méthodes de commodité comme `matches(pour:)` existent. Il est important de consulter la documentation de la bibliothèque tierce respective pour une syntaxe précise et la disponibilité des méthodes.
