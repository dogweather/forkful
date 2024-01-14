---
title:                "Clojure: Travailler avec des fichiers csv"
simple_title:         "Travailler avec des fichiers csv"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## Pourquoi

Travailler avec des fichiers CSV peut être une tâche essentielle pour de nombreux projets de programmation. Les fichiers CSV (Comma Separated Values) sont couramment utilisés pour stocker et échanger des données tabulaires, grâce à leur format simple et facilement lisible. Dans cet article, nous allons explorer comment travailler avec des fichiers CSV en utilisant Clojure.

## Comment faire

Pour commencer à travailler avec des fichiers CSV, nous allons utiliser la bibliothèque de traitement des données clojure.data.csv. Tout d'abord, importez cette bibliothèque dans votre fichier Clojure :

```Clojure
(ns mon-projet.core
  (:require [clojure.data.csv :as csv]))
```

Ensuite, nous pouvons lire un fichier CSV en utilisant la fonction `csv/read-csv`. Par exemple, si nous avons un fichier CSV nommé "donnees.csv" avec les colonnes "nom" et "age", nous pouvons le lire de la manière suivante :

```Clojure
(def donnees (with-open [file (clojure.java.io/reader "donnees.csv")]
                    (csv/read-csv file)))

;; Affiche le contenu du fichier
(println donnees) 
```

Cela retournera une liste contenant des vecteurs qui représentent chaque ligne du fichier, avec les valeurs des colonnes correspondantes. Pour accéder à une valeur spécifique, nous pouvons utiliser la fonction `get-in` comme ceci :

```Clojure
;; Accède au premier élément dans la première ligne du fichier
(get-in donnees [0 0])
```

Pour écrire des données dans un fichier CSV, nous pouvons utiliser la fonction `csv/write-csv` en spécifiant le nom du fichier et les données à écrire. Voici un exemple :

```Clojure
(def donnees-a-ecrire [["nom" "age"]
                       ["Jean" 25]
                       ["Marie" 30]])

;; Ecrit les données dans un fichier nommé "resultat.csv"
(csv/write-csv "resultat.csv" donnees-a-ecrire)
```

## Plongée en profondeur

Travailler avec des fichiers CSV peut devenir plus complexe lorsqu'il s'agit de gérer des données avec différents types et formats. Cependant, la bibliothèque clojure.data.csv propose des fonctions utiles pour aider à gérer ces situations. Par exemple, nous pouvons utiliser la fonction `parse-csv` pour lire un fichier CSV en spécifiant le type de données attendu pour chaque colonne. De plus, nous pouvons utiliser la fonction `write-csv` pour écrire des données en spécifiant les types de données pour chaque colonne.

## Voir aussi

- [Documentation officielle de la bibliothèque clojure.data.csv](https://clojure.github.io/clojure/clojure.data.csv-api.html)
- [Exemple complet de lecture et d'écriture de fichiers CSV en Clojure](https://github.com/VenmurasuS/Handling-CSV-in-Clojure)