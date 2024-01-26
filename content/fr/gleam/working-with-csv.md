---
title:                "Manipulation des fichiers CSV"
html_title:           "Bash: Manipulation des fichiers CSV"
simple_title:         "Manipulation des fichiers CSV"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Travailler avec des CSV, c’est manipuler des "Comma-Separated Values", des données tabulaires sous forme de texte. Les programmeurs l'utilisent pour facilement échanger des données entre des logiciels et des systèmes différents.

## How to:
```gleam
// Importez le module CSV si disponible
import gleam/csv

// Fonction de base pour lire un CSV
fn read_csv(data: String) -> Result(List(List(String)), String) {
  csv.parse(data)
}

// Exemple: lecture d'une chaîne CSV
fn main() {
  let data = "nom,âge\nAlice,30\nBob,25"
  case read_csv(data) {
    Ok(lines) -> lines
    Error(err) -> err
  }
}
```
Sortie attendue: `[["nom", "âge"], ["Alice", "30"], ["Bob", "25"]]`

## Deep Dive
Les CSV existent depuis les premiers jours de l’informatique personnelle, servant d’intermédiaire universel entre des systèmes hétérogènes. Alternativement, on utilise JSON ou XML pour plus de complexité structurelle. Dans Gleam, le parsing CSV demande de gérer les erreurs et la structure des données proprement, vu que le langage valorise la sûreté de type.

## See Also
- Documentation officielle de Gleam: [Gleam Lang](https://gleam.run/)
- Spécification de CSV: [RFC 4180](https://tools.ietf.org/html/rfc4180)
- Bibliothèque pour travailler avec CSV en Gleam (si disponible): [hex.pm](https://hex.pm/)
