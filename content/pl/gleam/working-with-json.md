---
title:                "Praca z json"
html_title:           "Gleam: Praca z json"
simple_title:         "Praca z json"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli masz do czynienia z danymi w formacie JSON (JavaScript Object Notation), warto wiedzieć, że Gleam jest idealnym narzędziem do ich przetwarzania. Dzięki wbudowanemu parserowi JSON i silnie typowanym funkcjom, programowanie w Gleam nie tylko ułatwi pracę z danymi, ale także poprawi bezpieczeństwo Twojego kodu.

## Jak to zrobić

Aby rozpocząć pracę z danymi JSON w Gleam, wystarczy wykorzystać wbudowany moduł ```json```. Możesz użyć funkcji ```parse``` do przekonwertowania ciągu znaków w formacie JSON na strukturę danych w Gleam. Na przykład:

```Gleam
let json_string = "{\"id\": 1, \"name\": \"John\"}"
let data = json.parse(json_string)
```

W przypisanym obiekcie ```data``` mamy teraz dostęp do pól ```id``` i ```name``` oraz odpowiednich wartości.

Jeśli chcesz stworzyć obiekt JSON z danymi w Gleam, możesz użyć funkcji ```encode```:

```Gleam
let data = (%{id: 2, name: "Jane"})
let json_string = json.encode(data)
```

W tym przypadku, zmienna ```json_string``` zawiera ciąg znaków w formacie JSON odpowiadający strukturze danych w Gleam. 

## Gleboka przygoda

W celu bardziej zaawansowanej obróbki danych JSON w Gleam, możemy wykorzystać funkcje z modułu ```Json.Decode``` i ```Json.Encode```. Na przykład, jeśli chcielibyśmy wyciągnąć pewne wartości z obiektu JSON i przekonwertować je na inny format, możemy skorzystać z funkcji ```map```:

```Gleam
let json_string = "{\"id\": 1, \"name\": \"John\"}"
let data = json.parse(json_string)
let name = Json.Decode.map(data, (\obj -> obj.name))
let upper_case_name = String.to_uppercase(name)
let json_name = Json.Encode.encode_string(upper_case_name)
```

W powyższym przykładzie, dla danych o strukturze ```{%{id: 1, name: "John"}}```, zmienna ```json_name``` przechowuje przekonwertowane dane w formacie JSON: ```"JOHN"```.

## Zobacz także

- Moduł ```json``` w dokumentacji Gleam: https://gleam.run/modules/json.html
- Przykładowe projekty i zastosowania JSON w Gleam: https://github.com/search?q=topic%3Ajson+org%3Agleam-lang&type=Repositories