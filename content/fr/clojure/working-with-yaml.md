---
title:                "Travailler avec YAML"
date:                  2024-01-19
simple_title:         "Travailler avec YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

YAML (YAML Ain't Markup Language) est un format de sérialisation de données lisible par l'homme utilisé pour la config, des fichiers de données, etc. Les développeurs l'emploient pour sa simplicité et sa facilité de compréhension par rapport à XML ou JSON.

## Comment ça marche ?

Pour travailler avec YAML en Clojure, on peut utiliser une librairie comme `clj-yaml`. Installez-le avec Leiningen : `[clj-yaml "0.7.0"]` ou avec Clojure CLI en ajoutant `clj-yaml {:mvn/version "0.7.0"}`.

Chargeons un fichier YAML :

```Clojure
(require '[clj-yaml.core :as yaml])

(defn lire-yaml [chemin]
  (with-open [r (io/reader chemin)]
    (yaml/parse-string (slurp r))))

(println (lire-yaml "config.yaml"))
```

Pour écrire des données en YAML :

```Clojure
(defn ecrire-yaml [donnees chemin]
  (spit chemin (yaml/generate-string donnees)))

(ecrire-yaml {:nom "Dupont"} "sortie.yaml")
```

Vérifiez le fichier `sortie.yaml` pour voir les résultats.

## Exploration approfondie

YAML a commencé en 2001, visant une facilité de lecture. JSON et XML étaient les alternatives, mais YAML offre une syntaxe moins verbeuse. En Clojure, `clj-yaml` s'appuie sur la librairie Java SnakeYAML pour le parsing et la génération. C’est crucial de gérer les nuances de YAML, y compris les types de données supportés et la représentation de structures complexes.

## Voir aussi

- Documentation officielle YAML : https://yaml.org/
- Repo GitHub `clj-yaml` : https://github.com/clj-commons/clj-yaml
- Une comparaison entre JSON, YAML et XML : https://phauer.com/2017/json-vs-xml-vs-toml-vs-cson-vs-yaml/
