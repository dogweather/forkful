---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:17.368162-07:00
description: "Jak to zrobi\u0107: Ruby domy\u015Blnie zawiera bibliotek\u0119 CSV,\
  \ kt\xF3ra upraszcza odczyt z i zapis do plik\xF3w CSV. Oto jak mo\u017Cesz to wykorzysta\u0107\
  \ do typowych zada\u0144."
lastmod: '2024-04-05T21:53:37.384213-06:00'
model: gpt-4-0125-preview
summary: "Ruby domy\u015Blnie zawiera bibliotek\u0119 CSV, kt\xF3ra upraszcza odczyt\
  \ z i zapis do plik\xF3w CSV."
title: Praca z plikami CSV
weight: 37
---

## Jak to zrobić:
Ruby domyślnie zawiera bibliotekę CSV, która upraszcza odczyt z i zapis do plików CSV. Oto jak możesz to wykorzystać do typowych zadań:

### Odczyt pliku CSV
Aby odczytać plik CSV, najpierw wymagana jest biblioteka CSV. Następnie można iterować po wierszach lub odczytać je do tablicy.

```ruby
require 'csv'

# Odczytywanie każdego wiersza jako tablicy
CSV.foreach("data.csv") do |row|
  puts row.inspect
end

# Wyjście dla każdego wiersza może wyglądać tak: ["data1", "data2", "data3"]
```

### Zapis do CSV
Zapis do pliku CSV jest również prosty. Możesz dopisywać do istniejącego pliku lub utworzyć nowy plik do zapisu.

```ruby
require 'csv'

CSV.open("output.csv", "wb") do |csv|
  csv << ["naglowek1", "naglowek2", "naglowek3"]
  csv << ["wartosc1", "wartosc2", "wartosc3"]
end

# To tworzy lub nadpisuje 'output.csv' z określonymi nagłówkami i wartościami.
```

### Parsowanie łańcucha CSV
Czasami musisz parsować dane CSV bezpośrednio z ciągu znaków. Oto jak:

```ruby
require 'csv'

data = "imie,wiek,miasto\nJan Kowalski,29,Nowy Jork\nAnna Nowak,31,Chicago"
csv = CSV.parse(data, headers: true)

csv.each do |row|
  puts "#{row['imie']} - #{row['wiek']} - #{row['miasto']}"
end

# Oczekiwane wyjście:
# Jan Kowalski - 29 - Nowy Jork
# Anna Nowak - 31 - Chicago
```

### Korzystanie z SmarterCSV
Do bardziej skomplikowanych zadań związanych z CSV, gem `SmarterCSV` może być cennym narzędziem. Najpierw zainstaluj gem:

```shell
gem install smarter_csv
```

Następnie możesz go użyć do obsługi dużych plików lub wykonywania bardziej zaawansowanego parsowania i manipulacji:

```ruby
require 'smarter_csv'

options = {}
data = SmarterCSV.process('duze_dane.csv', options)

data.each do |hash|
  puts hash.inspect
end

# To odczyta 'duze_dane.csv' i wyjście każdego wiersza jako hash bazujący na nagłówkach.
```

Podsumowując, wbudowana biblioteka CSV Ruby'ego wraz z gemami osób trzecich, takimi jak `SmarterCSV`, zapewnia solidne wsparcie dla obsługi danych CSV, umożliwiając wydajne zadania przetwarzania i manipulacji danymi.
