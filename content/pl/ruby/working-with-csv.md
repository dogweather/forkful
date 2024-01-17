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

## Czym jest i dlaczego to robimy?
Praca z plikami CSV jest powszechnym zadaniem dla programistów, szczególnie w kontekście importowania i eksportowania danych. Pozwala to na łatwe przenoszenie informacji pomiędzy różnymi aplikacjami i systemami. 

## Jak to zrobić?
Pierwszym krokiem jest wymagane zainstalowanie biblioteki CSV dla Ruby. Następnie możemy użyć metody ```CSV.foreach``` aby wczytać zawartość pliku CSV w naszym skrypcie. Wykorzystując opcję ```headers: true```, możemy również skorzystać z pierwszego wiersza jako nagłówka, co ułatwi nam późniejszą pracę z danymi. Przykładowy kod może wyglądać w ten sposób:

```ruby
require 'csv'
CSV.foreach("nazwa_pliku.csv", headers: true) do |row|
 puts row["nazwa_kolumny"]
end
```
W ten sam sposób możemy również zapisać dane do pliku CSV przy użyciu metody ```CSV.open```. Przykład:

```ruby
require 'csv'
CSV.open("nowy_plik.csv", "w") do |csv|
  csv << ["kolumna1", "kolumna2", "kolumna3"]
end
```

## Głębszy zanurzenie
Format CSV (Comma-Separated Values) został wprowadzony w latach 70-tych i od tego czasu stał się powszechnie używanym sposobem przechowywania prostych danych w postaci tabelarycznej. Alternatywą dla CSV mogą być na przykład pliki JSON czy XML, jednak CSV jest uważane za łatwiejsze w obsłudze i bardziej czytelne dla człowieka formatowanie. 

Biblioteka CSV zawiera również inne przydatne metody, takie jak ```CSV.read``` czy ```CSV.parse```, które pozwalają na bardziej precyzyjne zarządzanie danymi w pliku CSV. Szczegółowa dokumentacja i przykładowe użycie można znaleźć na oficjalnej stronie biblioteki: https://ruby-doc.org/stdlib-2.7.1/libdoc/csv/rdoc/CSV.html

## Zobacz również
- Oficjalna dokumentacja biblioteki CSV dla Ruby: https://ruby-doc.org/stdlib-2.7.1/libdoc/csv/rdoc/CSV.html
- Przykłady zastosowań plików CSV w Ruby: https://www.regular-expressions.info/csv.html