---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:13.807649-07:00
description: "Hvordan: Ruby inkluderer CSV-biblioteket som standard, noe som forenkler\
  \ lesing fra og skriving til CSV-filer. Her er hvordan du kan utnytte dette for\u2026"
lastmod: '2024-03-13T22:44:41.356142-06:00'
model: gpt-4-0125-preview
summary: Ruby inkluderer CSV-biblioteket som standard, noe som forenkler lesing fra
  og skriving til CSV-filer.
title: Arbeide med CSV
weight: 37
---

## Hvordan:
Ruby inkluderer CSV-biblioteket som standard, noe som forenkler lesing fra og skriving til CSV-filer. Her er hvordan du kan utnytte dette for vanlige oppgaver:

### Lese en CSV-fil
For å lese fra en CSV-fil, krever du først CSV-biblioteket. Deretter kan du iterere over rader eller lese dem inn i en matrise.

```ruby
require 'csv'

# Lese hver rad som en matrise
CSV.foreach("data.csv") do |rad|
  puts rad.inspect
end

# Utdata for hver rad kan se slik ut: ["data1", "data2", "data3"]
```

### Skrive til en CSV
Å skrive til en CSV-fil er også greit. Du kan legge til i en eksisterende fil eller opprette en ny fil for å skrive.

```ruby
require 'csv'

CSV.open("output.csv", "wb") do |csv|
  csv << ["header1", "header2", "header3"]
  csv << ["verdi1", "verdi2", "verdi3"]
end

# Dette oppretter eller overskriver 'output.csv' med de spesifiserte hodene og verdiene.
```

### Parse en CSV-streng
Noen ganger trenger du å parse CSV-data direkte fra en streng. Her er hvordan:

```ruby
require 'csv'

data = "navn,alder,by\nJohn Doe,29,New York\nJane Doe,31,Chicago"
csv = CSV.parse(data, med: true)

csv.each do |rad|
  puts "#{rad['navn']} - #{rad['alder']} - #{rad['by']}"
end

# Forventet utdata:
# John Doe - 29 - New York
# Jane Doe - 31 - Chicago
```

### Bruke SmarterCSV
For mer komplekse CSV-oppgaver, kan `SmarterCSV`-juvelen være et verdifullt verktøy. Først, installer juvelen:

```shell
gem install smarter_csv
```

Deretter kan du bruke den til å håndtere store filer eller utføre mer sofistikerte parsing og manipulasjon:

```ruby
require 'smarter_csv'

alternativer = {}
data = SmarterCSV.process('large_data.csv', alternativer)

data.each do |hash|
  puts hash.inspect
end

# Dette vil lese 'large_data.csv' og utgi hver rad som en hash basert på hodene.
```

Oppsummert gir Rubys innebygde CSV-bibliotek, sammen med tredjepartsjuveler som `SmarterCSV`, robust støtte for håndtering av CSV-data, noe som muliggjør effektive dataprosesserings- og manipuleringsoppgaver.
