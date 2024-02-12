---
title:                "Travailler avec YAML"
aliases:
- /fr/clojure/working-with-yaml/
date:                  2024-02-03T19:24:52.401814-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

YAML, un acronyme récursif pour "YAML Ain't Markup Language" (YAML n'est pas un langage de balisage), est un format de sérialisation de données lisible par l'homme utilisé pour les fichiers de configuration et l'échange de données entre les langues ayant différentes structures de données. Les programmeurs exploitent YAML en raison de sa simplicité et de sa lisibilité, ce qui en fait un choix idéal pour la configuration d'applications et la facilitation des échanges de données dans des environnements de programmation polyglottes.

## Comment faire :

Clojure n'inclut pas de support intégré pour YAML, mais vous pouvez utiliser des bibliothèques tierces comme `clj-yaml` pour analyser et générer des données YAML. Tout d'abord, ajoutez la bibliothèque à vos dépendances de projet :

```clojure
;; Ajoutez cela à vos dépendances de project.clj
[clj-yaml "0.7.0"]
```

Voici comment vous pouvez utiliser `clj-yaml` pour analyser YAML et convertir des maps Clojure en YAML.

### Analyser du YAML :

```clojure
(require '[clj-yaml.core :as yaml])

;; Analyse d'une chaîne YAML
(let [yaml-str "name: John Doe\nage: 30\nlanguages:\n  - Clojure\n  - Python"]
  (yaml/parse-string yaml-str))
;; Sortie :
;; => {"name" "John Doe", "age" 30, "languages" ["Clojure" "Python"]}
```

### Générer du YAML à partir de Clojure :

```clojure
(require '[clj-yaml.core :as yaml])

;; Convertir une map Clojure en une chaîne YAML
(let [data-map {:name "Jane Doe" :age 28 :languages ["Java" "Ruby"]}]
  (yaml/generate-string data-map))
;; Sortie :
; "age: 28\nlanguages:\n- Java\n- Ruby\nname: Jane Doe\n"
```

Ces opérations simples avec `clj-yaml` peuvent être intégrées dans des applications Clojure pour gérer des fichiers de configuration ou faciliter les échanges de données avec d'autres services ou composants qui utilisent YAML.
