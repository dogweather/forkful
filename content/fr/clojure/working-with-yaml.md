---
title:                "Clojure: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans le monde de la programmation, il y a souvent un besoin d'échanger des données entre différentes applications. L'une des façons courantes de le faire est en utilisant YAML. Ce langage de sérialisation de données est facile à lire pour les humains et peut être utilisé dans une variété de langages de programmation, y compris Clojure.

## Comment faire

Pour commencer à travailler avec YAML en Clojure, vous devez d'abord inclure la bibliothèque "clj-yaml" dans votre projet. Ensuite, vous pouvez utiliser des fonctions telles que "load-string" et "emit-string" pour lire et écrire des données YAML en clojure.

Voici un exemple de code qui charge un fichier YAML et en extrait une valeur spécifique :

```
(require '[clj-yaml.core :as yaml])

(def config (yaml/load-string "user: username"))
(println (:user config)) ; affiche : username
```

## Plongée profonde

Lorsque vous travaillez avec YAML en Clojure, il peut être utile de comprendre comment les données sont représentées sous forme de structures de données Clojure. Par exemple, une liste en YAML sera convertie en une liste Clojure, tandis qu'un objet sera représenté par une map avec des clés et des valeurs correspondantes.

Il est également important de comprendre comment gérer les données complexes telles que les données imbriquées ou les commentaires dans YAML. En explorant la documentation de la bibliothèque "clj-yaml" et en expérimentant avec différents exemples, vous pourrez mieux maîtriser cet outil puissant.

## Voir aussi

- [Documentation de la bibliothèque clj-yaml](https://github.com/gorillalabs/clj-yaml)
- [Exemples de YAML en Clojure](https://www.baeldung.com/clojure-yaml)
- [Tutoriel sur YAML en Clojure](https://www.oncrashreboot.com/use-yaml-babashka)