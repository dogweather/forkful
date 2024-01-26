---
title:                "Lavorare con i file CSV"
html_title:           "Bash: Lavorare con i file CSV"
simple_title:         "Lavorare con i file CSV"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Lavorare con i CSV significa gestire dati in formato "Comma-Separated Values", utile per dati tabellari. I programmatori lo fanno per importare, esportare e manipolare dati tra diversi sistemi e applicazioni.

## How to:
```Gleam
import gleam/io
import gleam/csv.{decode, Encode}

// Definire un record per gestire i dati
type Studente {
  Studente(nome: String, età: Int)
}

// Convertire da CSV a lista di record Studente
fn csv_a_studenti(csv_dati: String) -> Result(List(Studente), Nil) {
  csv_dati
  |> decode()
  |> list.map(fn(dati) {
    Studente(dati[0], dati[1].parse(Int) |> result.unwrap)
  })
}

// Esempio di utilizzo
fn main() {
  let dati_csv = "Mario,20\nLuigi,25"
  let studenti = csv_a_studenti(dati_csv)

  case studenti {
    Ok(studenti) -> io.println(studenti)
    Error(_) -> io.println("Errore nel parsing del CSV")
  }
}

// Output previsto
// [Studente("Mario", 20), Studente("Luigi", 25)]
```

## Deep Dive
Il CSV è nato negli anni '70 per facilitare il trasferimento di dati tra programmi diversi. A differenza di JSON o XML, CSV è più leggero ma con minore capacità di esprimere strutture complesse. Implementandolo in Gleam, si usa la libreria `gleam/csv`, che gestisce parsing e serializzazione. È importante gestire errori durante il parsing, data la possibile inconsistenza dei dati.

## See Also
- Introduzione al linguaggio Gleam: [https://gleam.run](https://gleam.run)
- Tutorial generici su CSV e programmazione: [https://www.programiz.com](https://www.programiz.com)
