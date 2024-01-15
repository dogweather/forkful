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

## Pourquoi

Tu te demandes peut-être pourquoi tu devrais utiliser des expressions régulières en Swift. Eh bien, laisse-moi te dire que ces petites lignes de code peuvent te faire gagner un temps précieux en manipulant et en filtrant des chaînes de caractères. Elles peuvent également améliorer la lisibilité de ton code en remplaçant de longues boucles et conditions par une seule ligne d'expression régulière. En bref, si tu travailles avec des chaînes de caractères, les expressions régulières sont un outil essentiel à connaître.

## Comment faire

La syntaxe des expressions régulières en Swift est très similaire à celle des autres langages. Tout d'abord, tu dois créer une instance du type `NSRegularExpression` en fournissant la chaîne de caractères de ton expression régulière et les options requises. Ensuite, tu peux utiliser la méthode `matches(in:options:range:)` pour trouver les correspondances dans une chaîne donnée. Voici un exemple pour extraire un numéro de téléphone en utilisant une expression régulière :

```
let phoneNumber = "+33 6 12 34 56 78" // chaîne donnée
let pattern = #"\+\d{2} \d{1} \d{2} \d{2} \d{2} \d{2} \d{2}"# // expression régulière

do {
    let regex = try NSRegularExpression(pattern: pattern, options: .caseInsensitive)
    let matches = regex.matches(in: phoneNumber,
                                options: [],
                                range: NSRange(location: 0, length: phoneNumber.utf16.count))
    if let match = matches.first {
        let numberRange = match.range // plage des caractères correspondants

        let startIndex = phoneNumber.index(phoneNumber.startIndex, offsetBy: numberRange.lowerBound)
        let endIndex = phoneNumber.index(phoneNumber.startIndex, offsetBy: numberRange.upperBound)
        let foundNumber = String(phoneNumber[startIndex...endIndex]) // extraction de la chaîne correspondante
        
        print(foundNumber) // imprime "+33 6 12 34 56 78"
    }
} catch {
    print("Erreur : \(error)")
}
```

## Plongée en profondeur

Maintenant que tu sais comment utiliser les expressions régulières en Swift, voici quelques astuces pour optimiser leur utilisation :

- Utilise les options `caseInsensitive` et `extended` pour faire correspondre les caractères en ignorant la casse et en utilisant des espaces et des commentaires dans ton expression.
- Utilise la fonction `replacingOccurrences(of:with:options:range:)` pour remplacer des parties spécifiques d'une chaîne en utilisant une expression régulière.
- Tu peux également utiliser des groupes dans ton expression pour extraire des parties spécifiques d'une chaîne et les utiliser dans le remplacement. Par exemple, si tu veux transformer un numéro de téléphone en un autre format, tu peux utiliser des groupes pour récupérer les différentes parties du numéro et les reconstituer différemment.

Maintenant à toi de jouer avec les expressions régulières pour découvrir toutes leurs possibilités !

## Voir aussi

- [Documentation officielle d'Apple](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Regex Cheat Sheet](https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/)
- [RegExr - Outil de test en ligne pour les expressions régulières](https://regexr.com/)