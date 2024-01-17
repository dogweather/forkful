---
title:                "Lavorare con i file csv"
html_title:           "Ruby: Lavorare con i file csv"
simple_title:         "Lavorare con i file csv"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

Lavorare con CSV (Comma Separated Values) significa manipolare dati in un formato tabellare attraverso il nostro codice. I programmatori spesso utilizzano CSV per importare ed esportare dati tra diverse applicazioni e per analizzarli in modo efficace.

## Come Fare:

```Ruby
require 'csv'

# Creare un nuovo CSV e scrivere dei dati al suo interno
CSV.open("nuovo_file.csv", "wb") do |csv|
  csv << ["Nome", "Cognome"]
  csv << ["Marco", "Rossi"]
  csv << ["Giulia", "Verdi"]
end

# Leggere un CSV esistente e stampare le sue righe
CSV.foreach("esempio.csv") do |row|
  puts row.inspect
end

# Trovare e selezionare dati specifici da un CSV
CSV.foreach("dati.csv", { headers: true, header_converters: :symbol, converters: :all }) do |row|
  puts row[:nome] if row[:cognome] == "Mori"
end
```

Esempio di output:

```
["Nome", "Cognome"]
["Marco", "Rossi"]
["Giulia", "Verdi"]
```

## Approfondimento:

L'utilizzo di CSV ha una lunga storia nella programmazione, risalente agli anni '70. Ci sono anche alternative come JSON o XML per la manipolazione dei dati, ma CSV è ancora molto popolare grazie alla sua semplicità e alla facilità di integrazione con molti programmi e linguaggi di programmazione.

Per lavorare con CSV è importante capire la struttura dei dati e utilizzare le giuste librerie o metodi per leggere e scrivere correttamente i dati. Inoltre, è possibile specificare opzioni come l'utilizzo di un separatore diverso dalla virgola o se il file contiene una riga di intestazione.

## Vedi Anche:

- La documentazione ufficiale di Ruby per la classe CSV: https://ruby-doc.org/stdlib-2.6.3/libdoc/csv/rdoc/CSV.html
- Un tutorial su come lavorare con CSV in Ruby: https://www.digitalocean.com/community/tutorials/how-to-work-with-csv-files-in-ruby
- Un confronto tra CSV, JSON e XML per la manipolazione dei dati: https://www.xplenty.com/blog/json-vs-xml-vs-csv/