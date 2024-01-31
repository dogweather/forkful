---
title:                "Manipulation de JSON"
date:                  2024-01-19
simple_title:         "Manipulation de JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Manipuler du JSON, c’est jouer avec le format standard pour échanger des données. Les programmeurs le font pour communiquer avec des APIs, stocker des données ou configurer des applications.

## How to (Comment faire)
```Swift
import Foundation

// Un JSON exemple
let jsonString = """
{
    "name": "Jean Dupont",
    "age": 31,
    "isDeveloper": true
}
"""

// Décode le JSON
struct User: Codable {
    var name: String
    var age: Int
    var isDeveloper: Bool
}

if let jsonData = jsonString.data(using: .utf8) {
    do {
        let user = try JSONDecoder().decode(User.self, from: jsonData)
        print(user)
    } catch {
        print("Erreur de décodage: \(error)")
    }
}
```
Sortie:
`User(name: "Jean Dupont", age: 31, isDeveloper: true)`

## Deep Dive (Plongée Profonde)
Le JSON (JavaScript Object Notation) est né de la nécessité de faciliter l'échange de données entre serveurs et clients. Avant, on utilisait XML, plus verbeux. Aujourd'hui, on a aussi YAML, plus lisible, mais JSON reste roi pour sa simplicité et sa rapidité. Pour l'implémentation, Swift fournit `JSONDecoder` et `JSONEncoder` pour faciliter la sérialisation et la désérialisation.

## See Also (Voir Aussi)
- Doc Swift officielle sur JSON: [Manipulation JSON](https://developer.apple.com/swift/)
- Guide Apple sur le travail avec le codeable: [Encoding and Decoding Custom Types](https://developer.apple.com/documentation/foundation/archives_and_serialization/encoding_and_decoding_custom_types)
- Comparaison JSON vs. XML: [JSON vs XML](https://www.w3schools.com/js/js_json_xml.asp)
