---
title:                "Arbeta med csv"
html_title:           "Arduino: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?
CSV (Comma-Separated Values) är en enkel textfilformat som används för att lagra tabelliknande data. Programmerare använder CSV för att enkelt utbyta data mellan olika program och system.

## Så här gör du:
```Ruby
require 'csv'

# Läsning av CSV-fil
CSV.foreach("exempel.csv") do |row|
  puts row.inspect
end

# Skrivning till CSV-fil
CSV.open("exempel_output.csv", "wb") do |csv|
  csv << ["Namn", "Ålder", "Stad"]
  csv << ["Alice", 29, "Stockholm"]
  csv << ["Bob", 42, "Göteborg"]
end
```
Sample output för läsningskoden kan vara `[["Alice", "29", "Stockholm"], ["Bob", "42", "Göteborg"]]`, medan skrivningskoden skapar en fil med angiven data.

## På djupet
CSV-formatet har använts sedan årtionden för att hantera data i textform; det är lätt att förstå och bearbeta både för människor och maskiner. Alternativ till CSV inkluderar JSON och XML, som båda stöder mer komplex datastrukturering. Med Ruby's standard CSV-bibliotek kan detaljer som anpassning av fältseparatörer och hantering av textkodningar enkelt konfigureras.

## Se också
- Ruby's officiella dokumentation om CSV: [Ruby CSV Documentation](https://ruby-doc.org/stdlib-2.6/libdoc/csv/rdoc/CSV.html)
- RFC 4180 om CSV-standard: [Common Format and MIME Type for CSV Files](https://tools.ietf.org/html/rfc4180)
- CSV på Wikipedia: [Comma-Separated Values on Wikipedia](https://en.wikipedia.org/wiki/Comma-separated_values)