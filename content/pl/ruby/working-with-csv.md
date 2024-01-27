---
title:                "Praca z plikami CSV"
date:                  2024-01-19
html_title:           "Bash: Praca z plikami CSV"
simple_title:         "Praca z plikami CSV"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Praca z CSV (Comma-Separated Values) to sposób zarządzania prostymi danymi tabularnymi. Programiści używają CSV dla łatwości wymiany danych między różnymi aplikacjami.

## How to:
Załaduj bibliotekę CSV:
```Ruby
require 'csv'
```

Odczytaj plik CSV:
```Ruby
CSV.foreach("ścieżka_do_pliku.csv", headers: true) do |row|
  puts row.to_h
end
```

Zapisz do pliku CSV:
```Ruby
CSV.open("ścieżka_do_wyjściowego_pliku.csv", "wb") do |csv|
  csv << ["Nagłówek1", "Nagłówek2"]
  csv << ["Wartość1", "Wartość2"]
end
```

Przykładowe wyjście:
```
{"Nagłówek1"=>"Wartość1", "Nagłówek2"=>"Wartość2"}
```

## Deep Dive
CSV pojawiło się w latach 70 i szybko stało się standardem w wymianie danych między systemami. Alternatywami są m.in. JSON, XML, ale CSV zachował popularność ze względu na prostotę. Ruby używa modułu `CSV` z biblioteki standardowej, który radzi sobie z różnymi konwencjami i formatami.

## See Also
- Dokumentacja Ruby CSV: [https://ruby-doc.org/stdlib-2.6.1/libdoc/csv/rdoc/CSV.html](https://ruby-doc.org/stdlib-2.6.1/libdoc/csv/rdoc/CSV.html)
- RFC 4180, definiujący CSV: [https://tools.ietf.org/html/rfc4180](https://tools.ietf.org/html/rfc4180)
- Ruby Guides na temat pracy z plikami CSV: [https://www.rubyguides.com/2018/10/parse-csv-ruby/](https://www.rubyguides.com/2018/10/parse-csv-ruby/)
