---
title:                "Praca z plikami csv"
html_title:           "Ruby: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego
CSV (ang. Comma Separated Values) to popularny format pliku używany do przechowywania danych w formie tabeli. Jest to idealny format dla programistów, którzy chcą łatwo przechowywać i przetwarzać dane w swoich aplikacjach. W tym artykule dowiesz się, jak używać Ruby do pracy z plikami CSV i jak możesz uniknąć zagwozdki związanych z przetwarzaniem danych w formacie tabeli.

## Jak to zrobić
```Ruby
require 'csv'

# Tworzenie nowego pliku CSV
CSV.open("plik.csv", "w") do |csv|
  csv << ["ID", "Imię", "Nazwisko"]
  csv << ["1", "John", "Kowalski"]
  csv << ["2", "Anna", "Nowak"]
end

# Odczytywanie danych z pliku CSV
CSV.foreach("plik.csv") do |row|
  puts row
end

# Modyfikowanie danych w pliku CSV
CSV.open("plik.csv", "a+") do |csv|
  csv << ["3", "Mark", "Smith"]
end
```

W powyższym przykładzie korzystamy z wbudowanej biblioteki CSV i jej metod `open` oraz `foreach` aby utworzyć nowy plik CSV, odczytać dane z istniejącego pliku i dodać nowe dane. Wykorzystujemy też operator `<<` aby dodać kolejne wiersze do pliku.

## Głębszy zanurzenie
Przy pracy z plikami CSV należy pamiętać o kilku ważnych rzeczach. Po pierwsze, należy uważać na to, żeby dane w pliku nie zawierały przecinków lub cudzysłowów, ponieważ mogą one powodować błędy podczas odczytywania pliku. Dodatkowo, warto używać odpowiednich opcji przy odczytywaniu danych (np. `headers: true` aby odczytać dane z pierwszego wiersza jako nagłówki kolumn). Możliwości manipulacji i przetwarzania danych w formacie CSV jest wiele i warto zapoznać się z dokumentacją biblioteki CSV oraz eksperymentować z różnymi metodami i opcjami.

## Zobacz też
- Dokumentacja biblioteki CSV: https://ruby-doc.org/stdlib-2.7.1/libdoc/csv/rdoc/CSV.html
- Poradnik: Jak pracować z CSV w Ruby: https://www.codecademy.com/articles/ruby-setup-csv