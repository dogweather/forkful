---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:07.381128-07:00
description: 'Come fare: Ruby include di default la libreria CSV, che semplifica la
  lettura e la scrittura di file CSV. Ecco come puoi sfruttarla per compiti comuni.'
lastmod: '2024-04-05T21:53:44.716182-06:00'
model: gpt-4-0125-preview
summary: Ruby include di default la libreria CSV, che semplifica la lettura e la scrittura
  di file CSV.
title: Lavorare con i CSV
weight: 37
---

## Come fare:
Ruby include di default la libreria CSV, che semplifica la lettura e la scrittura di file CSV. Ecco come puoi sfruttarla per compiti comuni:

### Leggere un file CSV
Per leggere da un file CSV, devi prima richiedere la libreria CSV. Poi, puoi iterare sulle righe o leggerle in un array.

```ruby
require 'csv'

# Leggendo ogni riga come un array
CSV.foreach("data.csv") do |row|
  puts row.inspect
end

# L'output per ogni riga potrebbe apparire così: ["data1", "data2", "data3"]
```

### Scrivere su un CSV
Scrivere su un file CSV è altrettanto semplice. Puoi aggiungere a un file esistente o crearne uno nuovo per scrivere.

```ruby
require 'csv'

CSV.open("output.csv", "wb") do |csv|
  csv << ["intestazione1", "intestazione2", "intestazione3"]
  csv << ["valore1", "valore2", "valore3"]
end

# Questo crea o sovrascrive 'output.csv' con le intestazioni e i valori specificati.
```

### Analizzare una stringa CSV
A volte è necessario analizzare i dati CSV direttamente da una stringa. Ecco come fare:

```ruby
require 'csv'

data = "name,age,city\nGiovanni Rossi,29,New York\nGianna Neri,31,Chicago"
csv = CSV.parse(data, headers: true)

csv.each do |row|
  puts "#{row['name']} - #{row['age']} - #{row['city']}"
end

# Output previsto:
# Giovanni Rossi - 29 - New York
# Gianna Neri - 31 - Chicago
```

### Utilizzare SmarterCSV
Per compiti CSV più complessi, il gem `SmarterCSV` può essere uno strumento prezioso. Prima, installa il gem:

```shell
gem install smarter_csv
```

Poi, puoi usarlo per gestire file di grandi dimensioni o eseguire parsing e manipolazioni più sofisticate:

```ruby
require 'smarter_csv'

options = {}
data = SmarterCSV.process('large_data.csv', options)

data.each do |hash|
  puts hash.inspect
end

# Questo leggerà 'large_data.csv' e produrrà l'output di ogni riga come un hash in base alle intestazioni.
```

Riassumendo, la libreria CSV integrata in Ruby, insieme a gem di terze parti come `SmarterCSV`, fornisce un robusto supporto per la gestione dei dati CSV, consentendo compiti efficienti di elaborazione e manipolazione dei dati.
