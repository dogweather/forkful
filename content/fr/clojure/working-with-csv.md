---
title:                "Manipulation des fichiers CSV"
date:                  2024-01-19
html_title:           "Bash: Manipulation des fichiers CSV"
simple_title:         "Manipulation des fichiers CSV"

category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
On traite des fichiers CSV pour manipuler des données tabulaires simple et facilement. C'est utile pour échanger des données entre systèmes ou applications distincts.

## How to:
### Lire un fichier CSV:
```Clojure
(require '[clojure.data.csv :as csv])
(require '[clojure.java.io :as io])

(with-open [reader (io/reader "chemin/du/fichier.csv")]
  (doall (csv/read-csv reader)))
```
Résultat (selon le contenu du fichier):
```Clojure
(["Header1" "Header2" "Header3"] ["Ligne1Col1" "Ligne1Col2" "Ligne1Col3"] ...)
```

### Écrire dans un fichier CSV:
```Clojure
(with-open [writer (io/writer "chemin/du/fichier.csv")]
  (csv/write-csv writer [["Colonne1" "Colonne2" "Colonne3"] ["Data1" "Data2" "Data3"]]))
```
Pas d'affichage direct, vérifier le fichier `chemin/du/fichier.csv`.

## Deep Dive
CSV a été créé dans les années 70. Le format est simple, mais pas standardisé, causant des variations dans son traitement (séparateur différent, échappement des caractères, etc.). JSON ou XML sont des alternatives modernes, mais CSV reste populaire pour sa compatibilité et sa facilité d'utilisation. En Clojure, on utilise souvent la bibliothèque `clojure.data.csv` pour son intégration bien pensée avec les séquences de Clojure.

## See Also
- Guide officiel de `clojure.data.csv`: [Clojure data.csv](https://github.com/clojure/data.csv)
- Spécifications CSV: [RFC 4180](https://tools.ietf.org/html/rfc4180)
- Comparaison des formats de données: [Quand utiliser JSON ou CSV](https://www.json.org/json-fr.html)
