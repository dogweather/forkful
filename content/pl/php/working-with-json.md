---
title:                "PHP: Praca z json"
simple_title:         "Praca z json"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

JSON jest jednym z najpopularniejszych formatów danych używanych w programowaniu PHP. Jest to prosty, lekki i czytelny sposób przechowywania i przesyłania danych. W tym artykule dowiesz się, dlaczego warto uczyć się programowania z wykorzystaniem JSON.

## Jak to zrobić

Aby zacząć pracę z JSON w PHP, musisz najpierw zaimportować odpowiednie funkcje za pomocą funkcji `json_encode()` i `json_decode()`. Następnie możesz zacząć konstruować swoje dane JSON i przesyłać je za pomocą `json_encode()`, a następnie je odczytywać i przetwarzać za pomocą `json_decode()`.

Przykład kodu:

```PHP
// Tworzenie i zapisywanie danych JSON
$cars = array(
  array(
    "marka" => "Ford",
    "model" => "Mustang",
    "rok produkcji" => 1998
  ),
  array(
    "marka" => "Chevrolet",
    "model" => "Camaro",
    "rok produkcji" => 2003
  ),
  array(
    "marka" => "Dodge",
    "model" => "Challenger",
    "rok produkcji" => 2012
  )
);

$json = json_encode($cars); // Konwersja danych na format JSON

// Wysyłanie danych JSON
echo "JSON data: " . $json;
```

Wynik:

```
JSON data: [{"marka":"Ford","model":"Mustang","rok produkcji":1998},{"marka":"Chevrolet","model":"Camaro","rok produkcji":2003},{"marka":"Dodge","model":"Challenger","rok produkcji":2012}]
```

Dzięki funkcji `json_encode()` możemy łatwo przekształcić nasze dane w format JSON, który jest już gotowy do przesyłania lub przechowywania.

Następnie możemy odczytać te dane za pomocą funkcji `json_decode()`, która przekształci je z powrotem na tablicę lub obiekt PHP. To pozwala nam na dalszą pracę z tymi danymi wewnątrz naszego kodu PHP.

Przykład kodu:

```PHP
// Odczytanie danych JSON i przetworzenie ich na tablicę
$cars_json = '[{"marka":"Ford","model":"Mustang","rok produkcji":1998},{"marka":"Chevrolet","model":"Camaro","rok produkcji":2003},{"marka":"Dodge","model":"Challenger","rok produkcji":2012}]';

$cars = json_decode($cars_json, true); // Ustawienie drugiego parametru na true spowoduje zwrócenie tablicy, a nie obiektu
```

Wynik:

```
Array
(
    [0] => Array
        (
            [marka] => Ford
            [model] => Mustang
            [rok produkcji] => 1998
        )

    [1] => Array
        (
            [marka] => Chevrolet
            [model] => Camaro
            [rok produkcji] => 2003
        )

    [2] => Array
        (
            [marka] => Dodge
            [model] => Challenger
            [rok produkcji] => 2012
        )
)
```

## Wnikliwiej

Podczas pracy z JSON w PHP warto pamiętać o kilku rzeczach:

- Jeśli Twoje dane do przetworzenia są prostą tablicą lub obiektem PHP, możesz użyć `json_encode()` i `json_decode()` bez ustawiania opcji.
- Jeśli jednak Twoje dane zawierają znaki specjalne, musisz użyć `json_encode()` z ustawieniem opcji `JSON_UNESCAPED_UNICODE`. W przeciwnym razie, znaki specjalne zostaną automatycznie uniesione, co może spowodować błędy przy przetwarzaniu danych.
- Jeśli chcesz sprawdzić, czy dane JSON są prawidłowo sformatowane, możesz użyć funkcji `json_last_error()` lub `json_last