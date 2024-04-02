---
date: 2024-01-26 04:26:35.031886-07:00
description: "TOML (Tom's Obvious, Minimal Language ou le Langage Minimal et \xC9\
  vident de Tom) est un format de s\xE9rialisation de donn\xE9es facile \xE0 lire\
  \ en raison de sa\u2026"
lastmod: '2024-03-13T22:44:58.253179-06:00'
model: gpt-4-0125-preview
summary: "TOML (Tom's Obvious, Minimal Language ou le Langage Minimal et \xC9vident\
  \ de Tom) est un format de s\xE9rialisation de donn\xE9es facile \xE0 lire en raison\
  \ de sa\u2026"
title: Travailler avec TOML
weight: 39
---

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
