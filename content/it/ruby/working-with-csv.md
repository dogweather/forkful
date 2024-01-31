---
title:                "Lavorare con i file CSV"
date:                  2024-01-19
html_title:           "Bash: Lavorare con i file CSV"
simple_title:         "Lavorare con i file CSV"

category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Il CSV (Comma-Separated Values) è un formato di file usato per memorizzare dati in forma tabellare. I programmatori lo utilizzano per scambiare dati tra sistemi diversi o per esportare dati da database e fogli di calcolo in modo semplice e leggibile.

## How to:
Ruby rende semplice lavorare con i file CSV usando la libreria standard CSV. Ecco un breve esempio per leggere e scrivere file CSV in Ruby.

```Ruby
require 'csv'

# Leggere un file CSV
CSV.foreach("dati.csv", headers: true) do |riga|
  puts "Nome: #{riga['Nome']}, Età: #{riga['Età']}"
end

# Creazione di un nuovo file CSV
CSV.open("nuovi_dati.csv", "wb") do |csv|
  csv << ["Nome", "Età"]
  csv << ["Paolo", "34"]
  csv << ["Lucia", "28"]
end
```

Output di esempio per la lettura:
```
Nome: Paolo, Età: 34
Nome: Lucia, Età: 28
```

## Deep Dive
I file CSV sono stati ampiamente usati fin dagli anni '70 per il trasferimento dati. Rispetto a formati come XML e JSON, il CSV ha la vantaggio di essere estremamente leggero e di facile lettura per l'uomo. Tuttavia, non è ottimale per dati complessi che richiedono una strutturazione gerarchica. In Ruby, la libreria CSV è in grado di gestire anche funzionalità avanzate come il parsing di CSV con campi incapsulati o separatori personalizzati.

## See Also
- Documentazione ufficiale Ruby per la libreria CSV: [ruby-doc.org](https://ruby-doc.org/stdlib-2.6/libdoc/csv/rdoc/CSV.html)
- Tutorial CSV in Ruby: [rubyguides.com](https://www.rubyguides.com/2018/10/parse-csv-ruby/)
- Passaggio da CSV a JSON in Ruby: [stackoverflow.com](https://stackoverflow.com/questions/19620642/automatically-transform-csv-to-json-in-ruby)
