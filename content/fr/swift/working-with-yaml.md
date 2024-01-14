---
title:                "Swift: Travailler avec YAML"
simple_title:         "Travailler avec YAML"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur ou une développeuse travaillant avec Swift, il est possible que vous ayez entendu parler de YAML. Ce format de données structuré est de plus en plus populaire dans le développement logiciel et peut être utile dans une variété de projets.

Si vous travaillez avec des fichiers de configuration, notamment avec des applications web ou mobiles, YAML peut être une solution pratique et efficace pour stocker et organiser des données.

## Comment faire

La première étape pour travailler avec YAML est d'importer la bibliothèque correspondante dans votre code Swift. Vous pouvez le faire en utilisant la commande `import YAML` dans votre fichier source. Ensuite, vous pouvez utiliser des fonctions telles que `YAMLEncoder` pour encoder des données en YAML ou `YAMLDecoder` pour décoder des données YAML en objets Swift.

Voici un exemple de code pour encoder et décoder une structure de données simple en YAML :

```Swift
import YAML

// Structure de données
struct Person {
    var name: String
    var age: Int
}

// Encodage en YAML
let person = Person(name: "Sophie", age: 26)
let encoder = YAMLEncoder()
let personYAML = try encoder.encode(person)

print(personYAML) // name: Sophie, age: 26

// Décodage en objet Swift
let decoder = YAMLDecoder()
let retrievedPerson = try decoder.decode(Person.self, from: personYAML)

print(retrievedPerson.name) // Sophie
print(retrievedPerson.age) // 26
```

## Plongée en profondeur

YAML est un format de données qui peut sembler similaire à JSON, mais il a ses propres spécificités et fonctionnalités. Par exemple, YAML permet l'utilisation de variables et de références, ce qui peut être utile pour éviter la répétition de données dans un fichier de configuration.

Il est également important de noter que YAML peut être sensible à la mise en forme. Les espaces et les tabulations ont une signification spécifique dans YAML, donc il est essentiel de les utiliser correctement lors de l'écriture de votre code.

Pour en savoir plus sur YAML et ses spécificités, vous pouvez consulter la documentation officielle ou des tutoriels en ligne.

## Voir aussi

Pour aller plus loin avec YAML et Swift, vous pouvez consulter les ressources suivantes :

- [Swift-YAML](https://github.com/behrang/YamlSwift) - bibliothèque YAML pour Swift
- [Guide de syntaxe YAML](https://yaml.org/spec/1.2/spec.html) - documentation officielle YAML
- [Apprendre YAML en 5 minutes](https://learnxinyminutes.com/docs/yaml/) - tutoriel rapide sur YAML

Nous espérons que cet article vous a donné un aperçu de l'utilisation de YAML avec Swift et que cela vous aidera dans vos projets de développement. À bientôt pour de nouveaux articles sur la programmation en Swift !