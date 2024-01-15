---
title:                "Arbeide med csv"
html_title:           "Ruby: Arbeide med csv"
simple_title:         "Arbeide med csv"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor bry seg om CSV? CSV (Comma-Separated Values) er et tekstformat som brukes til å lagre og håndtere store mengder data. Det er et utmerket verktøy for å organisere og analysere data i programmeringsspråket Ruby.

## Hvordan

CSV-filer kan leses og skrives med Ruby ved hjelp av standardbiblioteket "csv". Bruk følgende kode for å åpne en CSV-fil og lese data fra den:

```Ruby
require 'csv'
CSV.foreach("data.csv") do |row|
  puts row
end
```

Dette vil skrive ut hver rad i CSV-filen som et array. Du kan også spesifisere hvilke kolonner som skal skrives ut ved å bruke indeksering, for eksempel `row[0]` for å skrive ut første kolonne.

For å skrive data til en CSV-fil, må du først åpne en fil og deretter bruke `CSV::Writer` for å skrive ut dataene. Her er et eksempel på å skrive ut en liste med navn til en CSV-fil:

```Ruby
CSV.open("names.csv", "w") do |csv|
  csv << ["John", "Jane", "Bob"]
end
```

Dette vil opprette en CSV-fil med en rad og tre kolonner med navnene "John", "Jane" og "Bob".

## Dypdykk

Når du arbeider med CSV i Ruby, er det viktig å håndtere både innledende og avsluttende linjer, tomme felt og eventuelle spesielle tegn som kan føre til feil i filen.

En måte å håndtere disse problemene på er å bruke alternativet `headers: true` når du åpner en CSV-fil. Dette vil bruke den første linjen i filen som overskriftene for kolonnene, slik at du kan referere til dem ved navn i stedet for å bruke indeksering.

```Ruby
CSV.foreach("data.csv", headers: true) do |row|
  puts row["Name"]
end
```

For å håndtere tomme felt, kan du bruke en `unless`-setning for å sjekke om feltet er lik `nil` før du gjør noe med det. For å håndtere spesielle tegn, kan du umidlebart konvertere alle dataene til UTF-8-format ved å bruke `string.encode("UTF-8", "binary", invalid: :replace, undef: :replace, replace: '')`.

## Se også

- [Ruby's CSV Library](https://ruby-doc.org/stdlib-2.7.0/libdoc/csv/rdoc/CSV.html)
- [Ruby CSV Gem](https://github.com/ruby/csv)