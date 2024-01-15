---
title:                "Lavorare con i file CSV"
html_title:           "Ruby: Lavorare con i file CSV"
simple_title:         "Lavorare con i file CSV"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché

Se sei interessato a lavorare con dati tabellari, come ad esempio fogli di calcolo o file di database, allora dovresti considerare di utilizzare il formato CSV (Comma-Separated Values). Con Ruby, è possibile gestire facilmente file CSV, e ottenere i dati in un formato strutturato e leggibile.

## Come fare

Per iniziare a lavorare con CSV in Ruby, è necessario importare la libreria CSV nel tuo codice. Di seguito un esempio di come aprire e leggere un file CSV:

```Ruby
require 'csv'

CSV.foreach("file.csv") do |row|
  p row
end
```

Questo esempio utilizza il metodo `foreach` per leggere ogni riga del file CSV e stamparla a schermo. Se vuoi lavorare con una singola riga, puoi utilizzare il metodo `read`:

```Ruby
require 'csv'

row = CSV.read("file.csv").first
p row
```

Ora supponiamo che il nostro file CSV abbia una riga di intestazione, che indica il nome di ogni colonna di dati. Possiamo accedere facilmente ai dati di una specifica colonna utilizzando il nome della colonna anziché un indice numerico:

```Ruby
require 'csv'

CSV.foreach("file.csv", headers: true) do |row|
  puts row["Nome"].to_s + " ha " + row["Età"].to_s + " anni."
end
```

In questo esempio, stiamo utilizzando il metodo `foreach` insieme all'opzione `headers: true` per indicare che il nostro file CSV ha una riga di intestazione. Poi, stiamo stampando il nome e l'età di ogni persona presente nel file.

È anche possibile creare un nuovo file CSV con Ruby utilizzando il metodo `open` e specificando le colonne con il metodo `<<`:

```Ruby
require 'csv'

CSV.open("nuovo_file.csv", "w") do |csv|
  csv << ["Nome", "Cognome", "Età"]
  csv << ["Maria", "Rossi", 35]
  csv << ["Paolo", "Verdi", 27]
end
```

Al termine dell'esecuzione, avrai creato un nuovo file CSV con le tre colonne specificate e le informazioni inserite.

## Approfondimento

Oltre a leggere e scrivere file CSV in Ruby, ci sono alcune altre funzionalità utili da conoscere. Ad esempio, è possibile specificare il separatore del campo diverso dalla virgola utilizzando l'opzione `col_sep`:

```Ruby
CSV.foreach("file.csv", col_sep: ";") do |row|
  p row
end
```

Inoltre, se si sta lavorando con dati di grandi dimensioni, è possibile utilizzare il metodo `shift` per rimuovere righe dal file senza doverle leggere tutte:

```Ruby
CSV.foreach("file.csv", headers: true) do |row|
  break if row["Nome"] == "Giulia"
  puts row["Nome"].to_s + " ha " + row["Età"].to_s + " anni."
end
```

In questo esempio, stiamo leggendo il file CSV fino a quando non troviamo la riga con il nome "Giulia", momento in cui usciremo dal ciclo.

## Vedi anche

- [Documentazione ufficiale di Ruby per la classe CSV](https://ruby-doc.org/stdlib-2.7.1/libdoc/csv/rdoc/CSV.html)
- [Tutorial su come lavorare con CSV in Ruby](https://www.rubyguides.com/2019/05/working-with-csv-ruby/)
- [Articolo su come leggere e scrivere file CSV in Ruby](https://www.rubyguides.com/2018/10/csv-library-ruby/)