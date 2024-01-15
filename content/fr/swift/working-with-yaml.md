---
title:                "Travailler avec yaml"
html_title:           "Swift: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous faites du développement d'application iOS ou macOS, il est fort probable que vous ayez déjà entendu parler de YAML. Mais qu'est-ce que c'est et pourquoi serait-il utile pour vos projets ? YAML est un format de données simple et facile à lire qui peut être utilisé pour stocker des informations de configuration dans vos applications. Il est également très pratique pour décrire des objets de manière structurée, ce qui en fait un choix populaire pour les développeurs.

## Comment faire

Pour utiliser YAML dans votre projet Swift, il existe plusieurs options. Vous pouvez utiliser une librairie externe telle que Yams ou utiliser les fonctionnalités intégrées de Foundation. Voici un exemple simple pour écrire une structure dans un format YAML à l'aide de Foundation :

```Swift
import Foundation

// Définition de la structure
struct Person: Codable {
    let name: String
    let age: Int
    let profession: String
}

// Création d'une instance
let john = Person(name: "John", age: 25, profession: "Développeur")

// Encodage en YAML
let encoder = YAMLEncoder()
do {
    let data = try encoder.encode(john)
    if let yamlString = String(data: data, encoding: .utf8) {
        print(yamlString)
    }
} catch {
    print(error)
}

// La sortie sera la suivante :
// name: "John"
// age: 25
// profession: "Développeur"
```

Il est également possible de lire des données YAML à l'aide de Foundation. Voici un exemple pour décoder les données précédentes en une instance de la structure Person :

```Swift
import Foundation

// Définition de la structure
struct Person: Codable {
    let name: String
    let age: Int
    let profession: String
}

// Données YAML à décoder
let yamlData = """
name: "John"
age: 25
profession: "Développeur"
""".data(using: .utf8)

// Décodage en une instance de Person
let decoder = YAMLDecoder()
do {
    let john = try decoder.decode(Person.self, from: yamlData!)
    print(john)
} catch {
    print(error)
}

// La sortie sera la suivante :
// Person(name: "John", age: 25, profession: "Développeur")
```

## Plongeon plus profond

Mais comment fonctionne exactement l'encodage et le décodage YAML avec Foundation ? Foundation utilise des annotations pour indiquer comment encoder et décoder une structure en YAML. Par exemple, nous pouvons ajouter la clé `yaml` aux propriétés de notre structure pour indiquer comment elles doivent être représentées dans le format YAML :

```Swift
struct Person: Codable {
    let name: String
    let age: Int
    let profession: String
    
    // Annotations pour encoder en YAML
    let height: Double
    let isMarried: Bool
    
    enum CodingKeys: String, CodingKey {
        case name
        case age
        case profession
        case height = "taille"
        case isMarried = "estMarie"
    }
}

let john = Person(name: "John", age: 25, profession: "Développeur", height: 1.8, isMarried: true)

let encoder = YAMLEncoder()
do {
    let data = try encoder.encode(john)
    if let yamlString = String(data: data, encoding: .utf8) {
        print(yamlString)
    }
} catch {
    print(error)
}

// La sortie sera la suivante :
// name: "John"
// age: 25
// profession: "Développeur"
// taille: 1.8
// estMarie: true
```

En utilisant ces annotations, nous pouvons personnaliser notre encodage YAML selon nos besoins.

## Voir aussi

Pour en savoir plus sur l'utilisation de YAML avec Swift, vous pouvez consulter ces ressources utiles :

- [Documentation officielle de YAML](https://yaml.org)
- [Librairie Yams pour l'utilisation de YAML avec Swift](https://github.com/jpsim/Yams)
- [Article sur Medium expliquant l'utilisation de YAML avec Swift et Vapor](https://medium.com/flawless-app-stories/how-to-use-yaml-with-vapor-62aa6353efc9)