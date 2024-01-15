---
title:                "Travailler avec json"
html_title:           "Clojure: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## Pourquoi 

Si vous êtes un développeur à la recherche d'un moyen efficace de manipuler et de transférer des données, alors travailler avec des fichiers JSON en Clojure pourrait être la solution parfaite. En utilisant Clojure pour traiter des données JSON, vous pouvez bénéficier de la syntaxe concise du langage ainsi que de ses nombreuses bibliothèques et outils qui facilitent le traitement des données.

## Comment faire 

```Clojure (use '[clojure.data.json :as json])

;; Créer un fichier JSON avec des données
(def data {:nom "Jean" :âge 30 :sexe "homme"})

;; Convertir les données en format JSON
(json/write-str data)
;; Output : {"nom":"Jean","age":30,"sexe":"homme"}

;; Lire un fichier JSON et le convertir en données Clojure
(json/read-str "{\"nom\":\"Marie\",\"age\":\"25\",\"sexe\":\"femme\"}")
;; Output : {:nom "Marie", :âge 25, :sexe "femme"}
```

## Plongée en profondeur 

En travaillant avec Clojure, vous aurez également accès à des bibliothèques telles que `cheshire` et `clj-json` qui fournissent des fonctionnalités avancées pour la manipulation de données JSON. Vous pourriez également trouver utile d'utiliser des outils tels que `json-schema` pour valider la structure de vos fichiers JSON, ou `jsonista` pour simplifier la conversion de données en JSON. N'hésitez pas à explorer ces différentes options pour trouver celle qui correspond le mieux à vos besoins.

## Voir aussi 

- [Documentation officielle Clojure pour le traitement de données JSON](https://clojure.org/guides/json)
- [Tutoriel pour travailler avec des données JSON en Clojure](https://www.baeldung.com/clojure-json)
- [Comparaison des bibliothèques JSON en Clojure](https://www.dailycred.com/article/clojure-json-libraries-compared)