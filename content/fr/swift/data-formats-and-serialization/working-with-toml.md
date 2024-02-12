---
title:                "Travailler avec TOML"
aliases:
- fr/swift/working-with-toml.md
date:                  2024-01-26T04:26:35.031886-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/working-with-toml.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
TOML (Tom's Obvious, Minimal Language ou le Langage Minimal et Évident de Tom) est un format de sérialisation de données facile à lire en raison de sa sémantique claire. Les programmeurs utilisent TOML pour les fichiers de configuration où la lisibilité par les humains et l'analyse facile par les machines sont essentielles.

## Comment faire :
Pour commencer, vous avez besoin d'un analyseur TOML. Swift n'en possède pas un intégré, donc utilisons `TOMLDecoder`. Installez-le via le Swift Package Manager et ensuite sérialisez et désérialisez TOML facilement.

```Swift
import TOMLDecoder

let tomlString = """
title = "Exemple TOML"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
"""

struct Config: Codable {
    let title: String
    let owner: Owner
}

struct Owner: Codable {
    let name: String
    let dob: Date
}

let decoder = TOMLDecoder()
if let configData = tomlString.data(using: .utf8) {
    do {
        let config = try decoder.decode(Config.self, from: configData)
        print("Titre: \(config.title), Propriétaire: \(config.owner.name), Date de naissance: \(config.owner.dob)")
    } catch {
        print("Erreur lors de l'analyse TOML: \(error)")
    }
}
```

Ce code affiche :
```
Titre: Exemple TOML, Propriétaire: Tom Preston-Werner, Date de naissance: 1979-05-27 07:32:00 +0000
```

## Plongée en profondeur
TOML a été conçu par Tom Preston-Werner, le co-fondateur de GitHub, comme une alternative plus conviviale pour les humains par rapport à des formats tels que JSON ou YAML. Il vise la clarté, réduisant les chances de mauvaise interprétation par un humain ou une machine. En ce qui concerne les alternatives, YAML et JSON sont les suspects habituels, avec YAML penché vers la lisibilité humaine et JSON comme l'option plus simple et conviviale pour la machine. Lorsqu'on travaille avec TOML dans Swift, nous n'avons pas d'analyseur natif. Cependant, des bibliothèques tierces comme `TOMLDecoder` facilitent la conversion aisée entre les chaînes TOML et les types Swift, spécifiquement via les protocoles `Codable` introduits dans Swift 4 qui ont rationalisé la sérialisation.

## Voir Aussi
- La norme TOML : https://toml.io
- GitHub pour `TOMLDecoder` : https://github.com/dduan/TOMLDecoder
- Documentation Swift sur `Codable` : https://developer.apple.com/documentation/swift/codable
- Comparaison des formats de sérialisation de données : https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
