---
title:                "Współpraca z yaml"
html_title:           "Elixir: Współpraca z yaml"
simple_title:         "Współpraca z yaml"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego

Istnieje wiele formatów plików konfiguracyjnych, ale YAML jest jednym z najbardziej popularnych dla programistów. Oferuje on prostą składnię, łatwość czytania i umożliwia przechowywanie danych w hierarchicznej strukturze. W tym artykule dowiesz się, dlaczego warto używać YAML w swoich projektach i jak to zrobić.

## Jak to zrobić

Aby zacząć pracować z YAML w Elixir, musisz zainstalować bibliotekę `YAML` używając gestora pakietów Hex. Następnie musisz zaimportować funkcje z tej biblioteki do swojego pliku Elixir, aby móc wykorzystać jego możliwości.

```Elixir
defp deps do
  [{:yaml, "~> 0.2.2"}]
end

defmodule Example do
  import Yaml

  yaml = """
  name: John
  age: 30
  favorite_foods:
    - pizza
    - tacos
  """

  data = load(yaml) # wczytuje YAML jako mapę danych

  age = data["age"] # zwraca wartość klucza "age" jako integer

  favorite_foods = data["favorite_foods"] # zwraca wartość klucza "favorite_foods" jako listę

end
```

## Deep Dive

Jedną z najważniejszych funkcji YAML w Elixir jest funkcja `load/1`, która wczytuje YAML jako mapę danych. Jeśli chcesz skonwertować tę mapę na inny format, na przykład JSON, możesz użyć funkcji `encode/1`.

```Elixir
json = encode(data) # konwertuje mapę danych YAML na JSON
```

Ponadto, funkcja `load_file/1` pozwala na wczytanie danych bezpośrednio z pliku YAML. Jest to szczególnie przydatne, gdy masz duży plik YAML, który chcesz przetworzyć.

Jeśli potrzebujesz przykładowych plików YAML, możesz skorzystać z stron takich jak YAML.org lub skorzystać z gotowych szablonów YAML dostępnych dla różnych języków programowania.

## Zobacz także

- Dokumentacja Elixir YAML: https://hexdocs.pm/yaml/api-reference.html
- Przykłady YAML: https://yaml.org/start.html
- Szablony YAML dla różnych języków: https://yamltemplates.com/