---
title:                "Utiliser les expressions régulières"
html_title:           "Swift: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Quoi & Pourquoi?

Utiliser des expressions régulières est un moyen pour les programmeurs de rechercher et de manipuler des chaînes de texte en utilisant un modèle spécifique. Ils sont souvent utilisés pour valider les données saisies par les utilisateurs, pour rechercher des motifs dans de grands fichiers texte, ou pour transformer des données en un format spécifique.

# Comment faire :

Le code suivant montre comment rechercher un motif dans une chaîne de texte en utilisant des expressions régulières en Swift :

```Swift 
let input = "Bonjour le monde!" 
if let range = input.range(of: "le") { 
    print(input[range]) 
}
```
Lorsque vous exécutez ce code, la sortie sera «le».

Vous pouvez également utiliser des expressions régulières pour valider une adresse email en utilisant la méthode `matches()` :

```Swift 
func validateEmail(_ email: String) -> Bool { 
    let emailRegex = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,64}" 
    let emailPredicate = NSPredicate(format:"SELF MATCHES %@", emailRegex) 
    return emailPredicate.evaluate(with: email) 
}
```

# Plongée en profondeur :

Les expressions régulières ont été inventées dans les années 1950 par le mathématicien américain Stephen Kleene, mais elles sont toujours largement utilisées aujourd'hui dans le développement logiciel. Certains alternatives à l'utilisation des expressions régulières incluent les algorithmes de recherche de chaînes, les expressions de programmation, ou la manipulation de chaînes de caractères directement.

En utilisant des expressions régulières en Swift, vous devez importer le framework Foundation, qui fournit la classe NSRegularExpression pour travailler avec ces expressions.

# Voir aussi :

Pour en savoir plus sur l'utilisation des expressions régulières en Swift, vous pouvez consulter la documentation Apple officielle : https://developer.apple.com/documentation/foundation/nsregularexpression

Vous pouvez également trouver des ressources utiles pour approfondir vos connaissances sur les expressions régulières en consultant des sites Web tels que regex101.com ou regular-expressions.info.