---
title:                "Travailler avec les fichiers csv"
html_title:           "Gleam: Travailler avec les fichiers csv"
simple_title:         "Travailler avec les fichiers csv"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire ?
Travailler avec des fichiers CSV est une pratique courante en programmation, car les fichiers CSV (Comma-Separated Values) sont des formats de données tabulaires permettant de stocker et d'organiser facilement des informations dans un fichier texte. Les programmeurs utilisent souvent des fichiers CSV pour gérer des données telles que des listes d'utilisateurs, des données financières ou des données de vente.

## Comment faire :
```
Gleam.import("csv")

let csv_data = """
Name, Age, Country
John, 25, France
Maria, 30, Spain
Peter, 35, Germany
"""

let results = Gleam.csv.parse(csv_data)
```
Dans cet exemple, nous important d'abord le module CSV de Gleam, puis nous définissons une variable contenant nos données CSV. Enfin, nous utilisons la fonction ```parse``` du module CSV pour convertir nos données en une structure utilisable dans notre code. A partir de cela, nous pouvons procéder à la manipulation de nos données comme nous le souhaitons.

## Plongée en profondeur :
Le format CSV est largement utilisé depuis des années et est supporté par de nombreux logiciels. Il permet aux données d'être facilement éditées et échangées entre différentes applications. En alternative, il est possible d'utiliser d'autres formats de données tels que JSON ou XML. Cependant, ces formats peuvent être plus verbeux et moins adaptés pour les données tabulaires. En termes d'implémentation, le module CSV de Gleam utilise l'algorithme RFC 4180 pour vérifier la validité du fichier CSV.

## Voir aussi :
Pour en savoir plus sur le module CSV de Gleam, vous pouvez consulter la documentation officielle sur le site de Gleam. Vous pouvez également consulter des articles et des tutoriels sur l'utilisation de CSV en programmation pour approfondir vos connaissances.