---
title:                "Travailler avec yaml"
html_title:           "Kotlin: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi
Vous pourriez vous demander pourquoi travailler avec YAML, un langage de balisage léger. Une des raisons est que YAML est facile à lire et à écrire, ce qui en fait un choix populaire pour la configuration de fichiers de données.

## Comment faire
Pour commencer à travailler avec YAML en utilisant Kotlin, vous devrez d'abord ajouter la dépendance YAML à votre projet. Vous pouvez le faire en ajoutant la ligne suivante à votre fichier de configuration Gradle :
```
dependencies {
    implementation("net.revelc.code:kotlin-yaml:11.0.8")
}
```
À partir de là, vous pouvez utiliser les fonctionnalités de YAML dans votre code Kotlin en important la classe YAML. Par exemple, pour charger un fichier YAML, vous pouvez utiliser la fonction `load()` comme ceci :
```
val config = Yaml().load(File("config.yml").reader())
```
Vous pouvez également utiliser Kotlin pour générer des fichiers YAML en utilisant la fonction `dump()` :
```
val data = mapOf(
    "nom" to "John Doe",
    "âge" to 30,
    "ville" to "Paris"
)
val yaml = Yaml()
val output = yaml.dump(data)
println(output)
```
Cela produirait la sortie suivante :
```
nom: John Doe
âge: 30
ville: Paris
```

## Analyse approfondie
Maintenant que vous savez comment travailler avec YAML dans Kotlin, voici quelques informations supplémentaires pour approfondir votre compréhension. YAML signifie "YAML Ain't Markup Language" et a été créé pour être facile à utiliser pour les humains. Il est basé sur une structure de clés et de valeurs, avec l'utilisation d'indentations pour définir les niveaux de hiérarchie. Vous pouvez également utiliser des structures de données JSON pour représenter des données dans YAML.

## Voir aussi
- [Documentation officielle de Kotlin pour YAML](https://kotlinlang.org/docs/reference/other-formats.html#yaml)
- [Tutoriel sur la manipulation de fichiers YAML avec Kotlin](https://www.baeldung.com/kotlin-yaml)
- [Exemples de projets utilisant YAML en Kotlin](https://github.com/topics/kotlin-yaml)