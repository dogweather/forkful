---
title:                "Ruby: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli masz do czynienia z dużą ilością danych w formacie CSV, prawdopodobnie zastanawiasz się, co z nimi zrobić. Oto dlaczego warto nauczyć się programowania w Ruby i jak może on wynieść Twoją pracę z CSV na wyższy poziom.

## Jak to zrobić

W Ruby istnieje wiele gotowych narzędzi do pracy z danymi w formacie CSV. Przykładowo:

```Ruby
require 'csv'

# Wczytanie pliku CSV do tablicy
table = CSV.read("dane.csv")

# Iterowanie po wierszach i wyświetlanie danych
table.each do |row|
  puts "Kod produktu: #{row[0]}"
  puts "Nazwa produktu: #{row[1]}"
  puts "Cena: #{row[2]}zł"
end

```

Powyższy kod będzie czytał plik `dane.csv` i wyświetlał jego dane w czytelnej formie. Dzięki temu możesz szybko przeglądać i przetwarzać duże ilości informacji.

Możesz także użyć Ruby do tworzenia, modyfikowania i zapisywania plików CSV. Aby to zrobić, wystarczy użyć odpowiednich metod z biblioteki CSV. Na przykład:

```Ruby
require 'csv'

# Tworzenie pliku CSV z tablicy danych
table = [
  ["12345", "Kubek z nadrukiem", "12.99"],
  ["67890", "Podkładka pod mysz", "5.50"],
  ["13579", "Długopis z grawerem", "3.99"]
]

CSV.open("nowe_dane.csv", "wb") do |csv|
  table.each do |row|
    csv << row
  end
end

```

Dzięki temu możesz tworzyć własne pliki CSV bez konieczności korzystania z zewnętrznych narzędzi.

## Głębsze zanurzenie

Ruby oferuje wiele zaawansowanych technik do pracy z danymi w formacie CSV. Możesz na przykład zmieniać dane w trakcie ich przetwarzania, filtrować je po określonych kryteriach czy też łączyć z innymi źródłami informacji.

Ponadto, dzięki bibliotece `smarter_csv`, możesz dokładnie określić format danych w pliku CSV i automatycznie przetwarzać je na odpowiednie typy danych w Ruby.

Praca z CSV w Ruby może być także połączona z innymi narzędziami, takimi jak ActiveRecord, dzięki czemu możesz łatwo importować i eksportować dane do baz danych.

## Zobacz też

- [Dokumentacja Ruby dla modułu CSV](https://ruby-doc.org/stdlib-2.7.2/libdoc/csv/rdoc/CSV.html)
- [Biblioteka smarter_csv do zaawansowanego przetwarzania CSV](https://github.com/tilo/smarter_csv)
- [Przykłady użycia ActiveRecord z plikami CSV](https://guides.rubyonrails.org/active_record_import_export.html)