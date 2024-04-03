---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:18.499503-07:00
description: "Hur man g\xF6r: Ruby inkluderar CSV-biblioteket som standard, vilket\
  \ f\xF6renklar l\xE4sning fr\xE5n och skrivning till CSV-filer. S\xE5 h\xE4r kan\
  \ du utnyttja detta f\xF6r\u2026"
lastmod: '2024-03-13T22:44:38.454584-06:00'
model: gpt-4-0125-preview
summary: "Ruby inkluderar CSV-biblioteket som standard, vilket f\xF6renklar l\xE4\
  sning fr\xE5n och skrivning till CSV-filer."
title: Arbeta med CSV
weight: 37
---

## Hur man gör:
Ruby inkluderar CSV-biblioteket som standard, vilket förenklar läsning från och skrivning till CSV-filer. Så här kan du utnyttja detta för vanliga uppgifter:

### Läsa en CSV-fil
För att läsa från en CSV-fil, behöver du först inkludera CSV-biblioteket. Därefter kan du iterera över rader eller läsa in dem i en array.

```ruby
require 'csv'

# Läsa varje rad som en array
CSV.foreach("data.csv") do |row|
  puts row.inspect
end

# Utskriften för varje rad kan se ut så här: ["data1", "data2", "data3"]
```

### Skriva till en CSV
Att skriva till en CSV-fil är också rakt på sak. Du kan lägga till i en befintlig fil eller skapa en ny fil för att skriva.

```ruby
require 'csv'

CSV.open("output.csv", "wb") do |csv|
  csv << ["header1", "header2", "header3"]
  csv << ["value1", "value2", "value3"]
end

# Detta skapar eller skriver över 'output.csv' med de angivna rubrikerna och värdena.
```

### Tolkning av en CSV-sträng
Ibland behöver du tolka CSV-data direkt från en sträng. Så här gör du:

```ruby
require 'csv'

data = "name,age,city\nJohn Doe,29,New York\nJane Doe,31,Chicago"
csv = CSV.parse(data, headers: true)

csv.each do |row|
  puts "#{row['name']} - #{row['age']} - #{row['city']}"
end

# Förväntad utskrift:
# John Doe - 29 - New York
# Jane Doe - 31 - Chicago
```

### Använda SmarterCSV
För mer komplexa CSV-uppgifter kan `SmarterCSV`-paketet vara ett värdefullt verktyg. Först, installera paketet:

```shell
gem install smarter_csv
```

Därefter kan du använda det för att hantera stora filer eller utföra mer avancerad tolkning och manipulation:

```ruby
require 'smarter_csv'

options = {}
data = SmarterCSV.process('large_data.csv', options)

data.each do |hash|
  puts hash.inspect
end

# Detta läser 'large_data.csv' och skriver ut varje rad som en hash baserad på rubrikerna.
```

Sammanfattningsvis erbjuder Rubys inbyggda CSV-bibliotek, tillsammans med tredjeparts-paket som `SmarterCSV`, robust stöd för hantering av CSV-data, vilket möjliggör effektiva uppgifter för databearbetning och manipulation.
