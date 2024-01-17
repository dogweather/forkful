---
title:                "Travailler avec le JSON"
html_title:           "Kotlin: Travailler avec le JSON"
simple_title:         "Travailler avec le JSON"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?
Travailler avec JSON consiste à manipuler des données structurées au format JSON (JavaScript Object Notation) dans les applications. Les programmeurs le font pour faciliter l'échange de données entre différentes sources, telles que les serveurs et les clients, et pour pouvoir ensuite les traiter facilement dans leur code.

## Comment faire:
Utiliser JSON en Kotlin est simple et facile. Voici un exemple de code pour créer et lire un objet JSON:

```Kotlin
// Créer un objet JSON avec des paires de clé-valeur
val jsonData = jsonObject("nom" to "Jean", "âge" to 25)
// Lire et afficher la valeur de la clé "nom"
println(jsonData["nom"])
```
Sortie: "Jean"

Pour écrire un objet JSON dans un fichier, utilisez la fonction `writeToFile` avec le chemin du fichier en paramètre:

```Kotlin
val jsonData = jsonObject("nom" to "Jean", "âge" to 25)
writeToFile("/chemin/vers/fichier.json", jsonData)
```

## Plongée en profondeur:
JSON a été créé en 1999 comme alternative au format XML et est devenu l'une des méthodes les plus populaires pour transférer des données sur le web. Il est compatible avec la plupart des langages de programmation et est également largement utilisé dans les applications mobiles. D'autres formats de données, tels que YAML et CSV, peuvent également être utilisés pour les mêmes tâches, mais JSON reste le choix privilégié pour sa simplicité et sa flexibilité.

En Kotlin, JSON est géré par la bibliothèque kotlinx.serialization, qui fournit des annotations pour marquer les classes comme sérialisables en JSON. Cette bibliothèque prend aussi en charge la détection automatique des types de données, ce qui rend la manipulation de JSON encore plus facile.

## Voir aussi:
Pour en savoir plus sur l'utilisation de JSON en Kotlin, vous pouvez consulter la documentation officielle de kotlinx.serialization (https://github.com/Kotlin/kotlinx.serialization) et d'autres tutoriels en ligne. Vous pouvez également apprendre à utiliser JSON dans d'autres langages de programmation tels que JavaScript et Python pour une meilleure compréhension de son utilisation dans les applications.