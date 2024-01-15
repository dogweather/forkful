---
title:                "Travailler avec les fichiers CSV"
html_title:           "Clojure: Travailler avec les fichiers CSV"
simple_title:         "Travailler avec les fichiers CSV"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous avez probablement déjà travaillé avec des données tabulaires au format CSV (Comma-Separated Values). Peut-être avez-vous créé un fichier CSV à partir d'un tableur ou avez-vous utilisé des données CSV dans un projet de datavisualisation. Mais saviez-vous que Clojure offre des fonctionnalités puissantes pour travailler avec des fichiers CSV ? Dans cet article, nous allons voir pourquoi et comment intégrer les fichiers CSV dans vos projets Clojure.

## Comment faire

```Clojure
;;Importer la bibliothèque clojure.data.csv
(require '[clojure.data.csv :as csv])

;;Lire un fichier CSV et stocker les données dans une structure de données
(with-open [file (reader "chemin/vers/votre/fichier.csv")]
  (doall (csv/read-csv file)))
```

Le code ci-dessus utilise la bibliothèque "clojure.data.csv" pour lire un fichier CSV et stocker les données dans une liste de listes. Vous pouvez ensuite manipuler les données à l'aide des outils Clojure tels que "map" et "filter".

```Clojure
;;Manipuler les données d'un fichier CSV
(def données (with-open [file (reader "chemin/vers/votre/fichier.csv")]
                (doall (csv/read-csv file))))

;;Sélectionner uniquement certaines colonnes
(map #(select-keys % [:colonne1 :colonne2]) données)

;;Filtrer les lignes selon certaines conditions
(filter #(> (-> % first Integer/parseInt) 10) données)

;;Afficher les données au format table
(clojure.pprint/print-table données)
```

La bibliothèque "clojure.data.csv" offre également la possibilité d'écrire des données dans un fichier CSV avec "write-csv" et de spécifier le séparateur et l'encodage du fichier.

## Approfondissement

La bibliothèque "clojure.data.csv" offre des fonctionnalités avancées pour travailler avec des fichiers CSV. Il est possible de spécifier les options de lecture et d'écriture telles que le séparateur, l'encodage, les options de formatage et les gestionnaires d'erreurs. Vous pouvez également utiliser des transducteurs pour traiter les données plus efficacement.

Pour en savoir plus sur les différentes options et fonctionnalités de la bibliothèque, consultez la documentation officielle et les exemples de code sur le site https://github.com/clojure/data.csv.

## Voir aussi

- Documentation officielle de la bibliothèque "clojure.data.csv": https://clojure.github.io/data.csv/
- Exemples de code: https://github.com/clojure/data.csv/tree/master/examples