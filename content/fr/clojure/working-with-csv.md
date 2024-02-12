---
title:                "Travailler avec CSV"
aliases:
- fr/clojure/working-with-csv.md
date:                  2024-02-03T19:19:02.286540-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Travailler avec des fichiers CSV (Valeurs Séparées par des Virgules) implique l'analyse et la génération de données textuelles structurées sous forme de lignes et de colonnes, semblable aux données d'un tableur. Ce processus est essentiel pour l'échange de données entre applications, bases de données, et pour les tâches de transformation de données, en raison de l'adoption large du CSV comme format léger et interopérable.

## Comment faire :

### Lire un fichier CSV
Clojure n'a pas d'analyseur CSV intégré dans sa bibliothèque standard, mais vous pouvez utiliser la bibliothèque `clojure.data.csv` à cet effet. Tout d'abord, ajoutez la bibliothèque à vos dépendances de projet.

Dans votre `project.clj`, ajoutez la dépendance suivante :
```clojure
[clojure.data.csv "1.0.0"]
```
Pour lire un fichier CSV et imprimer chaque ligne :
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(with-open [reader (io/reader "chemin/vers/votrefichier.csv")]
  (doall
   (map println (csv/read-csv reader))))
```
Cela affichera chaque ligne du CSV comme un vecteur Clojure.

### Écrire dans un fichier CSV
Pour écrire des données dans un fichier CSV, vous pouvez utiliser la même bibliothèque `clojure.data.csv` :
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(let [data [["id" "name" "age"]
            ["1" "John Doe" "28"]
            ["2" "Jane Doe" "31"]]]
  (with-open [writer (io/writer "chemin/vers/fichierdesortie.csv")]
    (csv/write-csv writer data)))
```
Cela crée ou écrase `fichierdesortie.csv`, le remplissant avec les données spécifiées.

### Utiliser une bibliothèque tierce : `clojure.data.csv`

Bien que `clojure.data.csv` soit sans doute la bibliothèque la plus simple pour la manipulation de CSV dans Clojure, pour des tâches plus complexes, telles que la gestion de CSV contenant des caractères spéciaux ou des délimiteurs non conventionnels, vous pourriez explorer des options supplémentaires au sein de l'écosystème ou même considérer l'interopérabilité Java avec des bibliothèques telles qu'Apache Commons CSV. Cependant, pour la plupart des tâches de traitement de CSV standards dans Clojure, `clojure.data.csv` fournit un ensemble d'outils simples et efficaces.
