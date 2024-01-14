---
title:                "Ruby: Praca z formatem json"
simple_title:         "Praca z formatem json"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

JSON (JavaScript Object Notation) jest formatem danych często używanym w programowaniu. Jest to popularny wybór w przypadku wymiany danych między serwerem a klientem. W tym wpisie dowiesz się, dlaczego warto poznać JSON i jak zacząć pracę z tym formatem danych.

## Jak To Zrobić

### Tworzenie danych w formacie JSON

Aby stworzyć obiekt JSON w Ruby, musimy użyć metody `to_json`. Przykład:

```Ruby
person = {
  name: "Anna",
  age: 30,
  job: "Developer"
}

puts person.to_json
```

Output:

```JSON
{"name":"Anna","age":30,"job":"Developer"}
```

### Pobieranie danych z formatu JSON

Aby odczytać dane w formacie JSON w Ruby, używamy metody `JSON.parse`. Przykład:

```Ruby
json_string = '{"name":"Anna","age":30,"job":"Developer"}'
person = JSON.parse(json_string)

puts person["name"]
```

Output:

```Ruby
Anna
```

### Zapisywanie danych w formacie JSON

Możemy również zapisać dane w formacie JSON za pomocą metody `File.open` i `to_json`. Przykład:

```Ruby
person = {
  name: "Anna",
  age: 30,
  job: "Developer"
}

File.open("person.json", "w") do |f|
  f.write(person.to_json)
end
```

### Serializacja obiektów

W Ruby możemy również użyć modułu `ActiveModel::Serialization` do serializacji obiektów do formatu JSON. Należy pamiętać, że do prawidłowej serializacji obiektu, musimy dodać `include ActiveModel::Serialization` do naszej klasy. Przykład:

```Ruby
class Person
  include ActiveModel::Serialization
  attr_accessor :name, :age, :job

  def attributes
    {
      "name" => name,
      "age" => age,
      "job" => job
    }
  end
end

person = Person.new
person.name = "Anna"
person.age = 30
person.job = "Developer"

puts person.to_json
```

Output:

```JSON
{"name":"Anna","age":30,"job":"Developer"}
```

## Wgląd Głęboki

Aby uzyskać głębsze informacje na temat pracy z formatem JSON w Ruby, warto zapoznać się z dokumentacją biblioteki `json` oraz z dokumentacją modułu `ActiveModel::Serialization`.

## Zobacz również

- Dokumentacja biblioteki JSON: https://ruby-doc.org/stdlib-2.7.2/libdoc/json/rdoc/JSON.html
- Dokumentacja modułu ActiveModel::Serialization: https://apidock.com/rails/ActiveModel/Serialization