---
title:                "Praca z formatem json"
html_title:           "Ruby: Praca z formatem json"
simple_title:         "Praca z formatem json"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

JSON jest popularnym formatem danych w programowaniu i jest używany do przechowywania i wymiany danych. Wykorzystanie go w Twoich projektach może ułatwić komunikację między systemami, a także zwiększyć wydajność i elastyczność.

## Jak to zrobić

### Parsowanie danych JSON

Aby rozpocząć pracę z JSON w Ruby, możesz użyć wbudowanej klasy `JSON` i jej metod do parsowania danych. Przykładowo, jeśli masz plik `data.json` z danymi JSON, można je parsować w następujący sposób:

```Ruby
require 'json'

data = File.read('data.json')
parsed_data = JSON.parse(data)
```

Pierwsza linia importuje moduł `json`, a druga linia wczytuje zawartość pliku do zmiennej `data`. Następnie, wykorzystując metodę `parse` klasy `JSON`, parsujemy dane, a wynikiem jest obiekt Ruby.

### tworzenie i zapisywanie danych JSON

Aby stworzyć dane JSON z obiektów Ruby, możesz wykorzystać metodę `to_json`. Przykładowo, dla tablicy `pets`:

```Ruby
pets = ["dog", "cat", "bird"]
json_data = pets.to_json
```

Wynikiem jest ciąg znaków reprezentujący dane JSON: `["dog", "cat", "bird"]`.

Aby zapisać dane JSON do pliku, możesz użyć metody `dump` z modułu `json`:

```Ruby
File.open('pets.json', 'w') do |file|
  JSON.dump(pets, file)
end
```

Wynikowy plik będzie zawierał dane w formacie JSON: `["dog", cat", "bird"]`.

## Głębszy zanurzenie

Istnieje wiele metod i opcji do pracy z JSON w Ruby, w tym możliwość zmiany sposobu parsowania danych poprzez ustawienie opcji w metodzie `json` lub definiowanie własnych klas do obsługi specyficznych typów danych. Sprawdź dokumentację Ruby dla więcej szczegółów.

## Zobacz także

- Dokumentacja Ruby: https://www.ruby-lang.org/pl/
- Dokumentacja klasy JSON: https://ruby-doc.org/stdlib-2.7.0/libdoc/json/rdoc/JSON.html
- Przewodnik po parsowaniu danych JSON w Ruby: https://www.rubyguides.com/2019/05/ruby-json-tutorial/