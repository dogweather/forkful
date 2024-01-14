---
title:                "Ruby: Lavorare con i file csv"
simple_title:         "Lavorare con i file csv"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore Ruby, probabilmente hai sentito parlare di CSV. Ma perché dovresti impegnarti a lavorare con esso? CSV (Comma Separated Values) è un formato di file molto comune per l'archiviazione e l'importazione di dati tabellari. È molto utilizzato in ambito aziendale e in applicazioni web. Se stai svolgendo un progetto che richiede l'utilizzo di dati tabellari, come ad esempio un'e-commerce o un sistema di gestione degli ordini, lavorare con CSV potrebbe essere la soluzione migliore per te.

## Come fare

In Ruby, esistono diverse gemme (gem) disponibili per aiutarti a lavorare con i file CSV, come ad esempio "CSV" e "FasterCSV". Vediamo un esempio di come utilizzare la gemma "CSV" per leggere un file CSV e stampare il suo contenuto:

```
require 'csv'

CSV.foreach('file.csv') do |row|
  puts "#{row[0]} - #{row[1]} - #{row[2]}"
end

```

Output:

```
1 - Prodotto 1 - $10.00
2 - Prodotto 2 - $20.00
3 - Prodotto 3 - $30.00
```

In questo esempio, stiamo utilizzando il metodo "foreach" per iterare su ogni riga del nostro file CSV. Il blocco di codice all'interno del metodo viene eseguito per ogni riga del file. Utilizziamo la variabile "row" per accedere ai valori di ogni colonna. Nel nostro esempio, stiamo semplicemente stampando il contenuto del primo, secondo e terzo campo di ogni riga.

## Approfondimento

Oltre alla semplice lettura di un file CSV, è possibile utilizzare la gemma "CSV" per eseguire operazioni più complesse, come ad esempio scrivere su un file CSV o convertire i dati in un'applicazione web in un formato CSV. Puoi anche definire i separatori dei campi e le opzioni di formattazione per adattarli alle tue esigenze specifiche. Per ulteriori informazioni, consulta la documentazione ufficiale della gemma "CSV".

## Vedi anche

- Documentazione ufficiale della gemma CSV: https://ruby-doc.org/stdlib-3.0.0/libdoc/csv/rdoc/CSV.html
- Documentazione ufficiale della gemma FasterCSV: https://github.com/JEG2/faster_csv 
- Tutorial su come utilizzare la gemma CSV: https://www.rubyguides.com/2018/10/parse-csv-ruby/