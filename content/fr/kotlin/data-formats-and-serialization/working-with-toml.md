---
date: 2024-01-26 04:23:42.818858-07:00
description: "TOML signifie Tom's Obvious, Minimal Language (Langage Minimal et \xC9\
  vident de Tom). Il est utilis\xE9 pour les fichiers de configuration parce qu'il\
  \ est\u2026"
lastmod: '2024-03-13T22:44:57.766221-06:00'
model: gpt-4-0125-preview
summary: "TOML signifie Tom's Obvious, Minimal Language (Langage Minimal et \xC9vident\
  \ de Tom). Il est utilis\xE9 pour les fichiers de configuration parce qu'il est\u2026"
title: Travailler avec TOML
weight: 39
---

## Quoi & Pourquoi ?
TOML signifie Tom's Obvious, Minimal Language (Langage Minimal et Évident de Tom). Il est utilisé pour les fichiers de configuration parce qu'il est facile à lire et à écrire pour les humains, tout en étant facile à analyser pour les machines. Les développeurs se tournent vers TOML pour éviter l'encombrement de XML et les subtilités de JSON lors de la manipulation des configurations.

## Comment faire :
Pour gérer TOML en Kotlin, vous pourriez utiliser une bibliothèque comme `ktoml`. D'abord, ajoutons la dépendance dans votre `build.gradle.kts` :

```kotlin
dependencies {
    implementation("com.akuleshov7:ktoml:0.2.5")
}
```

Maintenant, analysons un peu de TOML :

```kotlin
import com.akuleshov7.ktoml.file.TomlFileReader

fun main() {
    val contenuToml = TomlFileReader.readAndParseFile("config.toml")
    
    val configurationBaseDeDonnées = contenuToml.getTable("database")
    val hôte = configurationBaseDeDonnées.getString("host")
    lePort = configurationBaseDeDonnées.getLong("port")

    println("Hôte de la base de données : $hôte")
    println("Port de la base de données : $lePort")
}
```

En supposant que `config.toml` ressemble à ceci :

```toml
[database]
host = "localhost"
port = 5432
```

Un exemple de sortie serait :

```
Hôte de la base de données : localhost
Port de la base de données : 5432
```

## Plongée Profonde
TOML, concocté par le co-fondateur de GitHub, Tom Preston-Werner, en 2013, visait à être plus simple que YAML et plus sûr en termes de types que JSON. Il est devenu un succès, notamment avec le `Cargo` de Rust et le système de modules de Go. Des alternatives ? YAML offre plus de fonctionnalités, JSON se traduit directement en objets dans de nombreux langages de programmation, et il y a toujours le bon vieux XML. En ce qui concerne l'implémentation, ktoml, sous licence Apache 2.0, est une bibliothèque pure Kotlin et n'embarque pas de bibliothèques Java, proposant également des DSL pour écrire en TOML, pas seulement pour lire.

## Voir Aussi
- Le GitHub de TOML : https://github.com/toml-lang/toml
- Le GitHub de ktoml : https://github.com/akuleshov7/ktoml
- TOML vs. YAML vs. JSON : https://blog.logrocket.com/comparing-configuration-files-yaml-toml-json/
