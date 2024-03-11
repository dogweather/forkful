---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:04.056018-07:00
description: "Travailler avec JSON en Swift signifie manipuler un format de donn\xE9\
  es l\xE9ger pour l'\xE9change de donn\xE9es. Les programmeurs utilisent JSON pour\
  \ transmettre\u2026"
lastmod: '2024-03-11T00:14:32.126486-06:00'
model: gpt-4-0125-preview
summary: "Travailler avec JSON en Swift signifie manipuler un format de donn\xE9es\
  \ l\xE9ger pour l'\xE9change de donn\xE9es. Les programmeurs utilisent JSON pour\
  \ transmettre\u2026"
title: Travailler avec JSON
---

{{< edit_this_page >}}

## Quoi et pourquoi ?

Travailler avec JSON en Swift signifie manipuler un format de données léger pour l'échange de données. Les programmeurs utilisent JSON pour transmettre des données entre un serveur et une application web car il est lisible et facile à analyser, tant pour les humains que pour les machines.

## Comment faire :

Swift simplifie l'analyse JSON avec le protocole `Codable`. Voici comment décoder du JSON en un objet Swift :

```Swift
import Foundation

// Définir un modèle conforme à Codable
struct Utilisateur: Codable {
    var nom: String
    var age: Int
}

// Chaîne JSON
let jsonString = """
{
    "nom": "John Doe",
    "age": 30
}
"""

// Convertir la chaîne JSON en Data
if let jsonData = jsonString.data(using: .utf8) {
    // Décoder les données JSON en objet Utilisateur
    do {
        let utilisateur = try JSONDecoder().decode(Utilisateur.self, from: jsonData)
        print("Nom: \(utilisateur.nom), Age: \(utilisateur.age)")
    } catch {
        print("Erreur lors du décodage du JSON: \(error)")
    }
}
```

Exemple de sortie :
```
Nom: John Doe, Age: 30
```

## Plongée profonde

JSON (JavaScript Object Notation) a été largement adopté depuis le début des années 2000, après avoir été spécifié par Douglas Crockford. Il a remplacé XML pour de nombreux cas d'utilisation en raison de sa syntaxe plus simple et de ses meilleures performances. Bien que `Codable` de Swift soit la méthode privilégiée pour JSON, des alternatives comme `JSONSerialization` existent pour les cas où l'on traite des types non conformes à Codable. Sous le capot, `Codable` abstrait l'analyse de bas niveau et rend la sérialisation/désérialisation sans couture.

## Voir aussi

- Explorez plus sur JSON et Swift sur le blog officiel de Swift : [Swift.org](https://swift.org/blog/)
- Consultez la documentation de `Codable` : [Swift Codable](https://developer.apple.com/documentation/swift/codable)
- Pour des structures JSON complexes, envisagez des bibliothèques tierces telles que SwiftyJSON disponibles sur [GitHub](https://github.com/SwiftyJSON/SwiftyJSON).
