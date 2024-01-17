---
title:                "Travailler avec yaml"
html_title:           "Clojure: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi?
Travailler avec YAML est une pratique courante chez les programmeurs pour stocker des données structurées de manière lisible par l'homme. YAML, ou Yet Another Markup Language, est un format de sérialisation de données qui permet de spécifier des objets et des valeurs de manière simple et intuitive. Cela facilite la manipulation et la communication des données entre les applications.

## Comment Faire:
Pour travailler avec YAML en Clojure, il existe plusieurs bibliothèques disponibles telles que "yaml-clojure" et "snakeyaml-clj". Voici un exemple de code montrant comment utiliser la bibliothèque "yaml-clojure" pour lire et écrire des données YAML:

```Clojure
(require '[clj-yaml.core :as yaml])

;; Écrire des données
(yaml/spit-string {"fruit" "pomme" "légumes" ["carotte" "laitue"]})

;; Lire des données
(yaml/parse-string "- banane
                    - fraise
                    - orange")
```

La sortie de ces exemples serait les données en format YAML, respectivement:

```YAML
fruit: pomme
légumes:
  - carotte
  - laitue

---
- banane
- fraise
- orange
```

## Deep Dive:
YAML a été créé en 2001 par Clark Evans et Ingy döt Net. Il est conçu pour ressembler à du texte natif et est souvent utilisé pour configurer des applications ou stocker des données de configuration. Il peut être utilisé pour remplacer des formats de données tels que le JSON ou le XML. YAML est également pris en charge par de nombreux langages de programmation, ce qui en fait un choix populaire pour les développeurs.

Une autre bibliothèque couramment utilisée pour travailler avec YAML en Clojure est "snakeyaml-clj". Elle offre des performances plus élevées que "yaml-clojure" mais peut nécessiter une configuration supplémentaire. Il est recommandé de comparer les performances des deux bibliothèques pour déterminer quelle serait la plus adaptée pour votre projet.

## Voir Aussi:
- Documentation de "yaml-clojure": https://github.com/yogthos/clj-yaml
- Documentation de "snakeyaml-clj": https://github.com/clj-commons/snakeyaml-clj
- Tutoriel sur YAML en Clojure: https://yogthos.net/posts/2018-07-07-Working-with-YAML-in-Clojure.html