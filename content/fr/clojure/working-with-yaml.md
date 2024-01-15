---
title:                "Travailler avec le yaml"
html_title:           "Clojure: Travailler avec le yaml"
simple_title:         "Travailler avec le yaml"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Clojure, vous avez probablement déjà entendu parler de YAML. Mais pourquoi vous engagez-vous dans cette technologie ? Et quelles sont les raisons pour lesquelles YAML est devenu si populaire ?

En fait, YAML (YAML Ain't Markup Language) est un langage de sérialisation de données qui est parfait pour les configurations de programmes et les fichiers de données structurées. Il est plus facile à lire que JSON et plus puissant que XML, ce qui en fait un choix idéal pour les utilisateurs de Clojure.

## Comment faire

Si vous voulez commencer à travailler avec YAML en utilisant Clojure, voici quelques étapes simples à suivre :

Tout d'abord, vous devez ajouter la dépendance suivante à votre projet Clojure :

```
[clojure-yaml "0.4.1"]
```

Ensuite, vous devrez importer la bibliothèque dans votre fichier :

```
(:require [clojure-yaml.core :as yaml])
```

Maintenant, vous pouvez utiliser les fonctions de cette bibliothèque pour lire et écrire des données YAML. Par exemple, pour lire des données YAML à partir d'un fichier, vous pouvez utiliser la fonction ```load-file``` :

```
(yaml/load-file "config.yml")
```

Et pour écrire des données YAML dans un fichier, vous pouvez utiliser la fonction ```spit``` :

```
(yaml/spit "nouveau-fichier.yml" data-to-write)
```

## Plongée profonde

Maintenant que vous savez comment utiliser YAML avec Clojure, voici quelques informations plus avancées pour vous aider à mieux comprendre cette technologie :

- YAML est un langage sensible à la casse, donc faites attention aux majuscules et aux minuscules dans vos clés et valeurs.
- Vous pouvez utiliser des structures de données Clojure telles que des vecteurs, des listes et des maps directement en YAML sans avoir à les convertir au préalable.
- Pour plus de personnalisation, vous pouvez également utiliser la bibliothèque ```clj-yaml``` pour contrôler la manière dont vos données sont converties en YAML.

## Voir aussi

- Site officiel YAML : https://yaml.org
- Wiki Clojure : https://github.com/clojure/clojure/wiki
- Exemple de projet Clojure utilisant YAML : https://github.com/seancorfield/yaml/clj-yaml