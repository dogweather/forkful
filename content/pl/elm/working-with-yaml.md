---
title:                "Praca z yaml"
html_title:           "Elm: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli chcesz pracować z plikami konfiguracyjnymi o prostym i zrozumiałym formacie, to YAML będzie świetnym wyborem. Jest to język znaczników, który jest popularny wśród programistów ze względu na swoją czytelność i elastyczność.

## Jak to zrobić

```Elm
import Json.Decode as Decode

data = """
name: Elm
type: language
version: 0.19
"""

yamlDecoder : Decode.Decoder (List ( String, String ))
yamlDecoder = Decode.list (Decode.pair Decode.string Decode.string)

result : Result Decode.Error (List (String, String))
result = Decode.decodeString yamlDecoder data

-- output: Ok [("name", "Elm"), ("type", "language"), ("version", "0.19")]
```

Aby rozpocząć pracę z YAML w Elm, musisz najpierw zaimportować bibliotekę `Json.Decode` i zdefiniować dekoder YAML, który w tym przykładzie konwertuje plik YAML na listę par klucz-wartość. Następnie wywołaj funkcję `decodeString` i podaj jej dekoder oraz wczytany plik YAML.

## Głębsze zanurzenie

Pliki YAML mogą zawierać wiele różnych danych, takich jak listy, mapy, liczby i ciągi znaków. W Elm możesz łatwo przekonwertować je na odpowiadające im typy danych, używając funkcji dekodujących z biblioteki `Json.Decode`. Jest również możliwe użycie zewnętrznych bibliotek, takich jak `avh4/elm-schema`, aby stworzyć bardziej szczegółowe dekodery, które pomogą w analizowaniu i walidacji plików YAML.

## Zobacz także

- [Oficjalna dokumentacja Elm](https://elm-lang.org/docs)
- [Repozytorium biblioteki Json.Decode na GitHubie](https://github.com/elm/json)
- [Strona YAML](https://yaml.org)