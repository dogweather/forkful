---
title:                "Swift: Utiliser des expressions régulières"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

#Pourquoi utiliser les expressions régulières en programmation Swift ?

Les expressions régulières sont une méthode de traitement de texte très utile en programmation Swift. Elles permettent de rechercher et de manipuler des motifs dans une chaîne de caractères, offrant une flexibilité et une puissance supplémentaires aux programmeurs. Voici comment les utiliser et les raisons pour lesquelles elles sont si populaires.

#Comment utiliser les expressions régulières en Swift

Les expressions régulières sont incluses dans le framework Foundation de Swift, ce qui les rend facilement accessibles pour les développeurs. Voici un exemple de recherche d'une adresse email dans une chaîne de caractères :

```Swift
var regex = try! NSRegularExpression(pattern: "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,64}", options: [])
let input = "Mon adresse email est example@email.com"
if let match = regex.firstMatch(in: input, options: [], range: NSRange(location: 0, length: input.utf16.count)) {
    let range = match.range
    let email = (input as NSString).substring(with: range)
    print(email) //Output: example@email.com
}
```

Ce code utilise une expression régulière pour rechercher une adresse email valide dans une chaîne de caractères. Vous pouvez également remplacer la chaîne de caractères "input" par une variable contenant du texte provenant d'une entrée utilisateur ou d'un fichier.

#Plongée en profondeur dans les expressions régulières

Les expressions régulières peuvent sembler compliquées au début, mais une fois que vous les avez comprises, elles peuvent être très utiles. Voici quelques conseils pour bien les utiliser :

- Utilisez des outils en ligne tels que Regex101 ou RegexBuddy pour tester vos expressions régulières avant de les implémenter dans votre code.
- Soyez attentif à l'utilisation des guillemets et des barres obliques inverses dans vos expressions régulières, car ils peuvent avoir un sens spécial en Swift.
- Utilisez les caractères spéciaux tels que "^" pour indiquer le début de la chaîne de caractères et "$" pour indiquer la fin de la chaîne de caractères.
- Utilisez des classes de caractères tels que "[A-Z]" pour indiquer un caractère alphabétique en majuscule.

Avec un peu de pratique, vous serez en mesure de créer des expressions régulières complexes pour résoudre divers problèmes de traitement de texte.

#Voir aussi

- [Documentation Apple pour les expressions régulières en Swift](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Vidéo "Les expressions régulières en 10 minutes" de Kilo Loco sur YouTube](https://www.youtube.com/watch?v=EkluES9Rvak)
- [Article "An Introduction to Regular Expressions in Swift" de Ben Scheirman](https://nsscreencast.com/episodes/005-regular-expressions-in-swift)