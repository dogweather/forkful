---
title:                "Lavorare con il csv"
html_title:           "Elixir: Lavorare con il csv"
simple_title:         "Lavorare con il csv"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti voler lavorare con i file CSV. Forse devi importare dati da un'altra fonte o esportare informazioni da una tabella o foglio di calcolo. Oppure stai lavorando con dati strutturati in formato tabellare e vuoi sfruttare la convenienza e la flessibilità dei file CSV.

Qualunque sia il motivo, Elixir offre un'ottima soluzione per lavorare con CSV in modo efficiente e semplice.

## Come fare

Per iniziare a lavorare con CSV in Elixir, la prima cosa da fare è includere il modulo CSV nella tua applicazione:

```Elixir
require CSV
```

A questo punto, puoi utilizzare le funzioni fornite dal modulo per leggere o scrivere file CSV. Ad esempio, per leggere un file CSV e stampare i suoi contenuti, possiamo utilizzare il seguente codice:

```Elixir
File.stream!("mio_file.csv") |> CSV.decode |> Enum.each(&IO.inspect/1) 
```

Questo codice usa la funzione `File.stream!` per aprire il file CSV, poi utilizza la funzione `CSV.decode` per decodificarlo e infine utilizza `Enum.each` per stampare ogni riga del file utilizzando la funzione `IO.inspect`.

Per scrivere su un file CSV invece, possiamo seguire questo esempio:

```Elixir
CSV.encode!(data, headers: [:nome, :cognome, :età], include_headers: true) |> File.write("mio_file.csv")
```

Questo codice utilizza la funzione `CSV.encode!` per convertire una lista di dati in un formato CSV, specificando anche i nomi delle colonne e l'inclusione dell'intestazione. Quindi, utilizzando la funzione `File.write`, il CSV viene scritto sul file.

## Approfondimento

Oltre alle funzioni di lettura e scrittura di base, Elixir offre una libreria `CSV` estremamente flessibile e potente per lavorare con i file CSV. Questa libreria supporta la lettura e la scrittura di CSV in diversi formati, come ad esempio CSV con valori separati da tabulazione, virgola o punto e virgola.

Inoltre, è possibile manipolare i dati CSV con molteplici opzioni, come la selezione di specifiche colonne o righe, l'ordinamento dei dati, la rimozione di righe duplicate e altro ancora.

Per saperne di più sulla libreria CSV in Elixir, consulta la documentazione ufficiale [qui] (https://hexdocs.pm/csv/).

## Vedi anche

- [Documentazione ufficiale di CSV in Elixir] (https://hexdocs.pm/csv/)
- [Libreria CSV su GitHub] (https://github.com/beatrichartz/csv)
- [Tutorial su come lavorare con CSV in Elixir] (https://www.poeticoding.com/csv-processing-in-elixir/)