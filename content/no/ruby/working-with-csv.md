---
title:                "Arbeid med CSV"
date:                  2024-01-19
simple_title:         "Arbeid med CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
CSV (Comma-Separated Values) er et filformat som bruker komma for å skille dataene. Programmerere jobber med CSV fordi det er enkelt, fleksibelt, og støttes av de fleste dataadministrasjonsverktøy.

## How to:
For å jobbe med CSV i Ruby, bruk 'csv'-biblioteket. Du kan lese, skrive og endre data enkelt.

```Ruby
require 'csv'

# Lese en CSV-fil
CSV.foreach('eksempel.csv', headers: true) do |rad|
  puts rad.to_hash
end

# Skrive til en ny CSV-fil
CSV.open('ny_eksempel.csv', 'wb') do |csv|
  csv << ['Navn', 'Alder']
  csv << ['Ola', 42]
  csv << ['Kari', 31]
end
```

Eksempel på utdata etter lesing av CSV:
```
{"Navn"=>"Ola", "Alder"=>"42"}
{"Navn"=>"Kari", "Alder"=>"31"}
```

## Deep Dive:
CSV har vært brukt siden 1970-tallet for å utveksle data mellom ulike programmer. Alternativer til CSV inkluderer JSON og XML, som begge kan bære mer komplekse data og strukturer. Ruby sin 'csv' biblioteket handler om å gjøre det enkelt å arbeide med CSV-filer og tilbyr funksjoner for både enkel og avansert manipulasjon av CSV-innhold.

## See Also:
- Ruby Dokumentasjon for CSV: [https://ruby-doc.org/stdlib-2.6.1/libdoc/csv/rdoc/CSV.html](https://ruby-doc.org/stdlib-2.6.1/libdoc/csv/rdoc/CSV.html)
- “Smarter CSV” Gem for mer kompleks CSV håndtering: [https://github.com/tilo/smarter_csv](https://github.com/tilo/smarter_csv)
- CSV på Wikipedia for mer historisk bakgrunn: [https://en.wikipedia.org/wiki/Comma-separated_values](https://en.wikipedia.org/wiki/Comma-separated_values)
