---
title:                "Travailler avec yaml"
html_title:           "Gleam: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

# Pourquoi

Écrire des fichiers de configuration peut souvent être fastidieux et sujet à des erreurs, en particulier lorsqu'il s'agit de fichiers de configuration volumineux et complexes. C'est là que YAML entre en jeu. YAML est un format de sérialisation de données léger et facilement lisible pour les humains, conçu pour faciliter la création et la manipulation de données structurées. En utilisant YAML, vous pouvez simplifier le processus de création et de gestion des fichiers de configuration.

# Comment faire

Pour commencer à utiliser YAML dans vos projets Gleam, vous pouvez suivre ces quelques étapes simples :

1. Installez le paquet "gleam-yaml" en utilisant la commande `gleam install gleam-lang/yaml`.
2. Importez le module YAML dans votre fichier Gleam en utilisant `import "yaml"`.
3. Utilisez la fonction `from_yaml` pour obtenir une représentation de vos données à partir d'un fichier YAML externe. Par exemple :

```
gleam> import "yaml"
gleam> let data = yaml.from_yaml("config.yml") // Si votre fichier de configuration s'appelle "config.yml"
```

4. Une fois que vous avez votre représentation de données, vous pouvez l'utiliser dans votre code Gleam comme bon vous semble. Par exemple, vous pouvez accéder à des valeurs spécifiques en utilisant des tuples de clés ou en utilisant des index numériques. Voici un exemple de code :

```
gleam> let data = yaml.from_yaml("config.yml")
gleam> let name = data.("name")
gleam> let version = data.("version")
gleam> let authors = data.("authors")
gleam> let first_author = authors.0
```

5. Lorsque vous avez terminé de travailler avec les données, vous pouvez les sérialiser à nouveau en utilisant la fonction `to_yaml`.

```
gleam> import "yaml"
gleam> let data = {(.) "name" ("Gleam"), (.) "version" (1), (.) "authors" (["Alice", "Bob", "Carol"])}
gleam> let data_yaml = yaml.to_yaml(data)
```

# Plongée profonde

Il y a d'autres fonctionnalités utiles fournies par le module YAML que vous pouvez explorer en utilisant la documentation ou en expérimentant par vous-même. Par exemple, vous pouvez utiliser la fonction `decode` pour convertir une chaîne YAML en un type personnalisé défini par l'utilisateur. Vous pouvez également utiliser la fonction `merge` pour fusionner deux structures de données YAML en une seule.

L'une des fonctionnalités les plus puissantes de YAML est la possibilité d'inclure des références à des fichiers externes dans votre fichier de configuration YAML. Cela peut être utile lorsque vous avez besoin de séparer vos données en différents fichiers pour une meilleure organisation et une meilleure lisibilité.

# Voir aussi

Pour en savoir plus sur YAML et comment l'utiliser dans vos projets Gleam, n'hésitez pas à consulter ces ressources supplémentaires :

- [Documentation de YAML sur le site Gleam](https://gleam.run/modules/yaml.html)
- [Exemple de projet Gleam utilisant YAML](https://github.com/gleam-lang/gleam-lang.github.io/blob/master/examples/yaml/src/yaml_app.gleam)
- [Guide officiel de YAML](https://yaml.org/spec/1.2/spec.html)