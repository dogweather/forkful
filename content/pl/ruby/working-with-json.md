---
title:                "Praca z json"
html_title:           "Ruby: Praca z json"
simple_title:         "Praca z json"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## O co chodzi? 
Praca z JSON-em jest nieodłączną częścią programowania w Ruby. W skrócie, JSON (JavaScript Object Notation) to format przechowywania i przesyłania danych, popularny wśród programistów ze względu na swoją prostotę, czytelność i zgodność z językiem JavaScript. Jest szeroko stosowany w aplikacjach internetowych, API oraz bazach danych.

Dlaczego stosujemy JSON-a? Otóż jest to bardzo wydajny sposób na przechowywanie i przesyłanie danych, ponieważ jest on oparty na lekkim i szybkim parserze. W porównaniu do innych formatów, takich jak XML, JSON jest znacznie bardziej przejrzysty i zwięzły, co ułatwia jego przetwarzanie przez programy.

## Jak to robić?
Przykłady kodów i ich wyników w Ruby zostały przedstawione poniżej. W Ruby istnieje wiele bibliotek umożliwiających pracę z JSON-em, jednak najpopularniejszą jest biblioteka `json`.

```Ruby
# Tworzenie obiektu JSON
json_object = {"imie":"Jan", "nazwisko":"Kowalski", age: 35}

# Konwersja na format JSON
json_string = json_object.to_json
puts json_string
# {"imie":"Jan", "nazwisko":"Kowalski", "age":35}

# Konwertowanie z formatu JSON do obiektu Ruby
ruby_object = JSON.parse(json_string)
puts ruby_object["nazwisko"]
# Kowalski
```

Możemy także wyświetlić ładnie sformatowany JSON, dodając parametr `pretty_generate`.

```Ruby
# Sformatowane wyświetlanie JSON
puts JSON.pretty_generate(ruby_object)
# {
#   "imie": "Jan",
#   "nazwisko": "Kowalski",
#   "age": 35
# }
```

## Głęboki zanurzenie
JSON powstał jako alternatywa dla formatu XML i szybko zyskał popularność w aplikacjach internetowych. Jego struktura jest bardzo podobna do tablic i obiektów w języku JavaScript, co sprawia, że jest ono łatwo przyswajalne dla programistów. Ponadto, JSON jest niewrażliwy na błędy, co oznacza, że nawet jeśli format jest niepoprawny, program nadal będzie w stanie odczytać i przetworzyć dane.

W Ruby, alternatywą dla biblioteki `json` jest gem `oj`, który jest znacznie szybszy od `json`, ale mniej czytelny dla ludzi.

W implementacji JSON-a ważną rolę odgrywa model danych `hash`, który pozwala na przechowywanie danych w formacie klucz-wartość.

## Zobacz także
Jeśli chcesz dowiedzieć się więcej o pracy z JSON-em w Ruby, zobacz poniższe źródła:
- [Dokumentacja Ruby o bibliotece json](https://ruby-doc.org/stdlib-2.6.6/libdoc/json/rdoc/JSON.html)
- [JSON w Ruby na przykładzie Railstutorial](https://www.railstutorial.org/book/filling_in_the_layout#sec-the_json_field_and_validation_errors)
- [10 sposobów na lepsze wykorzystanie biblioteki json w Ruby](https://www.telerik.com/blogs/10-reasons-to-use-json-in-ruby)