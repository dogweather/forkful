---
title:                "Gleam: Travailler avec des fichiers CSV"
simple_title:         "Travailler avec des fichiers CSV"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

# Pourquoi travailler avec des fichiers CSV en utilisant Gleam ?

Les fichiers CSV (Comma Separated Values) sont un type de format de fichier couramment utilisé pour stocker et organiser des données, en particulier des données tabulaires telles que des feuilles de calcul. La manipulation de ces fichiers est essentielle dans de nombreux projets de programmation et Gleam offre une solution simple et efficace pour travailler avec eux.

## Comment faire?

Voici un exemple de code pour lire un fichier CSV en utilisant Gleam :

```
Gleam.import.csv |> File.read("data.csv") |> Csv.Rows.decode
|> case Ok(rows) => rows
   Err(err) => panic(err)
```

Ce code utilise la bibliothèque standard de Gleam pour importer un module de CSV, puis il utilise la fonction `read` pour lire le contenu du fichier `data.csv`. Ensuite, il utilise une fonction de décodage pour convertir les données en un format utilisable par le programme. 

Pour écrire des données dans un fichier CSV, vous pouvez utiliser le code suivant :

```
ple.csv
import gleam/string

let rows = [
  \[Personne, Age, Profession],
  \[Jean, 25, “Ingénieur”],
  \[Marie, 32, “Professeur”],
  \[Luc, 41, “Médecin”]
]

gleam/string.join
|> Csv.Rows.encode(rows)
|> File.write("people.csv")
```

Ce code utilise la fonction `join` de la bibliothèque standard de Gleam pour convertir les données en un format CSV et les écrit dans le fichier "people.csv".

## Plongeons plus en profondeur

En travaillant avec des fichiers CSV, il est important de noter que les valeurs séparées par une virgule ne doivent pas contenir de virgules ou de guillemets, car cela peut provoquer des erreurs lors de la lecture ou de l'écriture des données. Les bibliothèques CSV de Gleam offrent également des fonctions pour gérer des caractères spéciaux si nécessaire.

Il est également possible d'utiliser la fonction `Csv.Rows.decode_with_headers` pour traiter un fichier CSV avec une ligne d'en-tête, qui contient les noms des colonnes. Cela peut faciliter la manipulation des données, car vous pouvez accéder aux valeurs en utilisant le nom de colonne plutôt que leur position dans la ligne.

# Voir aussi

Voici quelques liens utiles pour en savoir plus sur la manipulation de fichiers CSV avec Gleam :

- [Documentation de la bibliothèque CSV de Gleam](https://gleam.run/modules/g/csv/latest/)
- [Exemples de code pour travailler avec des fichiers CSV en utilisant Gleam](https://github.com/search?q=gleam+csv)