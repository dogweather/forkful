---
title:                "Praca z yaml"
html_title:           "Haskell: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

# Co & Dlaczego?

Praca z YAML to sposób na przechowywanie i przetwarzanie danych w czytelnej i łatwej do zrozumienia formie. Programiści korzystają z niego, ponieważ ułatwia to tworzenie i konfigurację aplikacji oraz ułatwia utrzymanie i udostępnianie danych.

## Jak to zrobić:
Przykładowe kody i wyniki można zestawić w bloki kodu ```Haskell ... ```

### Przetwarzanie danych YAML:
```Haskell
import Data.Yaml
main = do
  input <- readFile "input.yaml"
  let decoded = decode input :: Maybe Value
  case decoded of
    Just (Object obj) -> print $ obj .: "klucz"
    _ -> putStrLn "Błąd: Nie udało się zdekodować danych YAML"
```

Wynik:
```
Wartość klucza w pliku input.yaml: wartość
```

### Tworzenie danych YAML:
```Haskell
import Data.Yaml
main = do
  let value = object [ "klucz" .= "wartość" ]
      encoded = encode value
  writeFile "output.yaml" encoded
```

Wynik (plik output.yaml):
```yaml
klucz: wartość
```

## Głębsza analiza:
YAML został stworzony w 2001 roku jako język szablonów dla języka Perl. Obecnie jest szeroko stosowany w wielu językach programowania, w tym w Haskellu. Alternatywami dla YAML są na przykład JSON lub XML. Implementacja dla języka Haskell dostępna jest w bibliotece ```Data.Yaml```.

## Zobacz też:
- [Data.Yaml dokumentacja](https://hackage.haskell.org/package/yaml)
- [JSON - alternatywny format danych](https://www.json.org/)
- [XML - alternatywny format danych](https://www.w3.org/XML/)