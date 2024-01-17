---
title:                "Arbeid med csv"
html_title:           "Ruby: Arbeid med csv"
simple_title:         "Arbeid med csv"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

##Hva & hvorfor?

Å jobbe med CSV (Comma Separated Values) er en måte å lagre og håndtere data på i et lettleselig format. Det er spesielt nyttig for programmerere å bruke når de trenger å organisere og manipulere store mengder informasjon. CSV-filer kan enkelt leses og skrives med Ruby, noe som gjør det til et populært valg blant utviklere.

##Hvordan:

La oss se på en enkel måte å lese og skrive til en CSV-fil med Ruby:

```ruby
require 'csv'

# Les en fil og lagre dataene som et array
data = CSV.read('data.csv')

# Legg til en ny rad med data
data << ['Jane', 22, 'New York']

# Skriv dataene tilbake til filen
CSV.open('data.csv', 'w') do |csv|
  data.each do |row|
    csv << row
  end
end

# Skriver ut dataene til konsollen
puts data
```

Her har vi brukt `CSV`-modulen som følger med Ruby for å lese og skrive til en CSV-fil. Vi begynner med å lese inn en fil og lagre dataene som et array. Deretter legger vi til en ny rad med data til vårt array og til slutt skriver vi disse dataene tilbake til filen. Ved å skrive ut arrayet får vi se den siste raden vi la til.

Output:
```ruby
[['John', 25, 'London'], ['Maria', 31, 'Paris'], ['Jane', 22, 'New York']]
```

##Dypdykk:

CSV-formatet har eksistert siden slutten av 1900-tallet og har blitt et standardformat for å lagre og utveksle data. Selv om Ruby gjør det enkelt å jobbe med CSV-filer, er det også andre biblioteker og verktøy som kan brukes til å behandle disse filene, som for eksempel `FasterCSV` og `Pandas` for Python.

Når det gjelder implementering av CSV i Ruby, bruker `CSV`-modulen en enkel og effektiv metode kalt "Row-oriented I/O" for å lese og skrive data i en CSV-fil. Denne metoden innebærer å behandle dataene rad for rad, i motsetning til å lese hele filen på en gang.

##Se også:

- [Ruby-dokumentasjonen for CSV](https://ruby-doc.org/stdlib-3.0.0/libdoc/csv/rdoc/CSV.html)
- [FasterCSV](https://github.com/JEG2/faster_csv)
- [Pandas](https://pandas.pydata.org/)