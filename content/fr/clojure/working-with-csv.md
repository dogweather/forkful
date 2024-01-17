---
title:                "Travailler avec les fichiers csv"
html_title:           "Clojure: Travailler avec les fichiers csv"
simple_title:         "Travailler avec les fichiers csv"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

# Quoi & Pourquoi ?
CSV (Comma-Separated Values) est un format de fichier largement utilisé en programmation pour stocker des données tabulaires, telles que des feuilles de calcul ou des bases de données. Les programmeurs utilisent souvent ce format car il est simple, naïf et facile à manipuler, ce qui en fait un excellent choix pour un large éventail de tâches.

# Comment faire :
Voici un exemple de code Clojure pour lire et écrire des fichiers CSV :

```clojure
;; Importez le module `clojure.java.io` pour accéder aux fonctionnalités d'entrée/sortie.

(require '[clojure.java.io :as io])

;; Définissez le nom du fichier CSV à lire ou écrire.

(def filename "mon_fichier.csv")

;; Pour écrire dans un fichier CSV :
 
(with-open [f (io/writer filename)]
  (clojure.csv/write-csv f [["Colonne 1" "Colonne 2"] ; la première ligne peut contenir des en-têtes de colonnes
                            ["Donnée 1" "Donnée 2"]
                            ["Donnée 3" "Donnée 4"]]))

;; Pour lire depuis un fichier CSV :

(with-open [r (io/reader filename)]
  (doseq [row (clojure.csv/read-csv r)]
    (println row))) ; chaque ligne du fichier CSV est retournée comme une liste de valeurs

```

## Deep Dive :
CSV a été inventé en 1972 par Bob Bemer comme moyen flexible de partager des données entre différents programmes. Il est maintenant largement reconnu comme un format de fichier standard pour les données tabulaires. Alternativement, certains programmeurs préfèrent utiliser des formats de données plus structurés, tels que JSON ou edn, pour stocker des données. La bibliothèque officielle de Clojure pour travailler avec CSV est `clojure.csv`.

## Voir aussi :
- Documentation officielle de Clojure pour `clojure.csv`: https://clojure.github.io/data.csv/
- Un tutoriel utile pour travailler avec des données CSV en Clojure : https://www.braveclojure.com/working-with-csv/