---
title:                "Kotlin: Travailler avec yaml."
simple_title:         "Travailler avec yaml."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

# Pourquoi utiliser YAML en Kotlin

Si vous travaillez avec des données structurées dans votre code Kotlin, vous avez probablement rencontré des formats tels que JSON et XML. Mais avez-vous déjà entendu parler de YAML ? Voici pourquoi vous devriez considérer l'utilisation de YAML dans votre programmation.

## Comment faire

Pour commencer à utiliser YAML en Kotlin, vous devrez d'abord ajouter la dépendance suivante à votre fichier build.gradle :

```
implementation 'org.yaml:snakeyaml:1.27'
```

Ensuite, importez la classe `Yaml` dans votre fichier Kotlin :

```
import org.yaml.snakeyaml.Yaml
```

Pour sérialiser un objet en YAML, utilisez la méthode `dump()` de la classe `Yaml`. Par exemple, pour sérialiser une liste de livres :

```
val books = listOf("Harry Potter", "Lord of the Rings", "The Hunger Games")
val yaml = Yaml()
val booksYaml = yaml.dump(books)
```

Cela produira une chaîne YAML comme celle-ci :

```
- Harry Potter
- Lord of the Rings
- The Hunger Games
```

Pour désérialiser une chaîne YAML en objet, utilisez la méthode `load()` de la classe `Yaml`. Par exemple, pour désérialiser une liste de langages de programmation :

```
val yaml = Yaml()
val languagesYaml = "- Java\n- Python\n- Kotlin"
val languages = yaml.load<List<String>>(languagesYaml)
```

Cela créera une liste de langages contenant les strings "Java", "Python" et "Kotlin".

## Plongée approfondie

YAML est un format de données simplifié et facile à lire, qui est souvent utilisé pour les fichiers de configuration. Il est également très flexible et peut représenter des données complexes avec facilité. Contrairement à XML et JSON, YAML ne nécessite pas de balises ou de caractères d'échappement, ce qui le rend plus facile à écrire et à lire pour les humains.

En utilisant les classes et méthodes de la bibliothèque SnakeYAML, vous pouvez facilement intégrer YAML dans votre code Kotlin et l'utiliser pour des tâches telles que la gestion de configurations ou le stockage de données.

## Voir aussi

- [Site officiel de YAML](https://yaml.org/)
- [Documentation de la bibliothèque SnakeYAML](https://bitbucket.org/asomov/snakeyaml/wiki/Documentation)
- [Tutoriel vidéo sur l'utilisation de YAML en Kotlin](https://www.youtube.com/watch?v=0yLnQZFCj0Y)