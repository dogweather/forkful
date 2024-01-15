---
title:                "Travailler avec yaml"
html_title:           "Elm: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur ou une programmeuse, il est possible que vous soyez déjà familier·ère avec le format de données YAML. Mais si vous ne l'êtes pas encore, ne vous inquiétez pas ! Ce format est de plus en plus utilisé dans le monde du développement web et de nombreux langages de programmation le prennent en charge. C'est pourquoi il peut être utile d'apprendre à travailler avec YAML dans votre propre langage de prédilection, comme Elm.

## Comment faire

Pour commencer à travailler avec YAML en Elm, vous devez d'abord installer le package `elm-exploration/yaml`. Ensuite, vous pouvez importer la bibliothèque dans votre code en utilisant la déclaration `import Yaml`. Maintenant, vous êtes prêt·e à convertir des données YAML en valeurs Elm et vice versa.

Voici un exemple de code pour transformer une chaîne YAML en un type de donnée Elm :

```Elm
myString = "name: John Doe age: 30"
result = Yaml.fromString myString
```

Le résultat de cette expression sera un `Result` contenant un `Value` avec les clés "name" et "age" ainsi que leurs valeurs correspondantes.

Si vous voulez créer une chaîne YAML à partir d'un type de données Elm, voici un exemple :

```Elm
person = { name = "Jane Doe", age = 25 }
result = Yaml.toString person
```

Le résultat de cette expression sera une chaîne contenant les clés "name" et "age" ainsi que les valeurs correspondantes du tableau `person`.

## Plongée en profondeur

Maintenant que vous avez appris à manipuler des données YAML avec Elm, il peut être utile de connaître quelques astuces supplémentaires. Par exemple, si vous avez besoin de spécifier un format personnalisé pour convertir des valeurs en chaînes YAML, vous pouvez utiliser la fonction `Yaml.Encode.encode` avec un `Yaml.Value`.

De plus, vous pouvez également utiliser des types de données personnalisés en utilisant `Json.Decode` pour décoder des valeurs en JSON et les convertir ensuite en YAML.

## Voir aussi

Pour en savoir plus sur la manipulation de données YAML en Elm, vous pouvez consulter les liens suivants :

- La documentation officielle du package `elm-exploration/yaml` : [lien](https://package.elm-lang.org/packages/elm-exploration/yaml/latest/)
- Un tutoriel vidéo sur l'utilisation de YAML en Elm : [lien](https://www.youtube.com/watch?v=WhpTrg2eSyU)
- Un exemple pratique utilisant l'intégration de YAML dans un projet Elm : [lien](https://github.com/dillonkearns/elm-yaml)