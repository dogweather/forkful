---
title:                "Travailler avec json"
html_title:           "Swift: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/working-with-json.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Travailler avec JSON est un moyen pour les programmeurs de partager et de stocker des données structurées dans un format léger et facile à lire. Cela permet également aux applications d'échanger des informations entre elles de manière efficace.

## Comment faire:

```Swift
// Création d'un objet JSON
let json: [String: Any] = [
    "nom": "Jean",
    "âge": 35,
    "ville": "Paris"
]

// Conversion en données JSON
if let jsonData = try? JSONSerialization.data(withJSONObject: json, options: .prettyPrinted) {
    // Conversion en chaîne de caractères
    if let jsonString = String(data: jsonData, encoding: .utf8) {
        // Affichage du résultat
        print(jsonString)
    }
}
```

Voici le résultat de l'exemple ci-dessus:

```Swift
{
    "nom": "Jean",
    "âge": 35,
    "ville": "Paris"
}
```

## Plonger plus profondément:

JSON, ou JavaScript Object Notation, a été développé par Douglas Crockford dans les années 1990 et est devenu un format de données populaire pour de nombreuses applications web et mobiles. Il existe également d'autres alternatives telles que XML, mais JSON est souvent privilégié en raison de sa simplicité et de sa compatibilité avec de nombreuses langues de programmation.

L'implémentation de JSON peut être réalisée à l'aide de nombreuses bibliothèques et frameworks, tels que SwiftyJSON, Codable ou encore CodablePlus. Ces outils facilitent la manipulation de données JSON et peuvent être utiles pour les projets de grande envergure.

## Voir aussi:

Pour en savoir plus sur JSON et comment l'utiliser en Swift, voici quelques sources utiles:

- [La documentation officielle de Swift sur le traitement de JSON](https://docs.swift.org/swift-book/LanguageGuide/CollectionTypes.html#//apple_ref/doc/uid/TP40014097-CH8-ID346)
- [Un tutoriel vidéo sur l'utilisation de SwiftyJSON](https://www.youtube.com/watch?v=MQEL2OSvDZM)
- [Un article sur les différentes bibliothèques de traitement de JSON en Swift](https://medium.com/@olton/processing-json-with-swift-5ef83ed2e826)