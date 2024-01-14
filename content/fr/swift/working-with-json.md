---
title:                "Swift: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/working-with-json.md"
---

{{< edit_this_page >}}

## Pourquoi

JSON est un format de données très courant utilisé pour échanger des informations entre différentes applications. En tant que programmeur Swift, vous pourriez être amené à travailler avec des données JSON pour implémenter des fonctionnalités de réseau ou pour stocker des données dans une base de données distante.

## Comment faire

Le moyen le plus simple d'interagir avec JSON en Swift est d'utiliser la classe `JSONDecoder`. Voici un exemple de code pour décoder des données JSON à partir d'une URL :

```Swift
let url = URL(string: "https://exemple.com/donnees.json")!
let donnees = try Data(contentsOf: url)
let decoder = JSONDecoder()
let objetJSON = try decoder.decode(MonObjet.self, from: donnees)
```

Ce code utilise `JSONDecoder` pour décoder les données JSON et les convertir en une instance d'objet Swift. Vous pouvez ensuite utiliser l'objet comme vous le feriez avec n'importe quel autre objet Swift.

Si vous souhaitez créer des données JSON à partir d'un objet Swift, vous pouvez utiliser la classe `JSONEncoder` :

```Swift
let objet = MonObjet(nom: "John Doe", age: 30)
let encoder = JSONEncoder()
let donnees = try encoder.encode(objet)
```

## Plongée en profondeur

Travailler avec JSON implique souvent de manipuler des données imbriquées ou des tableaux. Pour cela, vous pouvez utiliser des fonctions de haut niveau de la classe `JSONDecoder`, telles que `decodeIfPresent(_:forKey:)` ou `decode(_:forKey:)`, pour extraire des valeurs spécifiques de votre JSON.

Vous pouvez également personnaliser la manière dont votre objet est encodé en implémentant le protocole `Codable` et en définissant les fonctions `encode(to:)` et `init(from:)`. Cela peut être utile si vous avez besoin de gérer des cas spécifiques lors de l'encodage ou du décodage de vos données JSON.

## Voir aussi

- [Guide officiel de Swift sur la manipulation de données JSON](https://docs.swift.org/swift-book/LanguageGuide/AdvancedOperators.html)
- [Documentation Apple sur la classe `JSONDecoder`](https://developer.apple.com/documentation/foundation/jsondecoder)
- [Documentation Apple sur la classe `JSONEncoder`](https://developer.apple.com/documentation/foundation/jsonencoder)