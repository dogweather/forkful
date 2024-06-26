---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:22.109846-07:00
description: "Comment faire : La prise en charge native de Swift pour les regex utilise\
  \ la classe `NSRegularExpression`, ainsi que les m\xE9thodes de plage et de\u2026"
lastmod: '2024-03-13T22:44:58.206110-06:00'
model: gpt-4-0125-preview
summary: "La prise en charge native de Swift pour les regex utilise la classe `NSRegularExpression`,\
  \ ainsi que les m\xE9thodes de plage et de remplacement de la classe String."
title: "Utilisation des expressions r\xE9guli\xE8res"
weight: 11
---

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
