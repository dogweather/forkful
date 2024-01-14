---
title:                "Elixir: Praca z plikiem YAML"
simple_title:         "Praca z plikiem YAML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, dlaczego warto poznać język programowania Elixir? Jednym z powodów może być fakt, że Elixir jest doskonałym narzędziem do pracy z plikami YAML. W tym artykule dowiesz się, dlaczego warto zacząć pracować z YAML w Elixir i jak to zrobić.

## Jak

Elixir oferuje wiele narzędzi do pracy z plikami YAML, ale najważniejszym z nich jest biblioteka `YAML`. Aby zacząć, musisz dodać ją do swojego projektu, do pliku `mix.exs`:

```Elixir
defp deps do
  [{:yaml, "~> 0.4.7"}]
end
```

Następnie zaimportuj bibliotekę w swoim pliku Elixir:

```Elixir
import YAML
```

Teraz możesz ładować pliki YAML i pracować z nimi w swoim kodzie. Na przykład, jeśli masz plik `config.yml` z taką treścią:

```YAML
names:
  - Adam
  - Ewa
  - Jan
  - Anna
```

Możesz go wczytać do zmiennej w ten sposób:

```Elixir
names = YAML.load_file("config.yml")
```

Aby uzyskać dostęp do zawartości pliku, możesz skorzystać z funkcji `get/3`:

```Elixir
names = YAML.load_file("config.yml")

# Pobierz pierwsze imię
first_name = names |> get("names") |> hd()

# Pobierz ostatnie imię
last_name = names |> get("names") |> List.last()
```

## Deep Dive

Jak już widzisz, praca z YAML w Elixir jest bardzo prosta. Biblioteka `YAML` oferuje wiele funkcji, na przykład `load_string/1` do ładowania ciągów tekstu YAML, `dump/1` do zapisywania danych w formacie YAML oraz `get/3` do pobierania wartości z pliku.

Jeśli chcesz dowiedzieć się więcej na temat pracy z YAML w Elixir, możesz przejrzeć dokumentację biblioteki na stronie [hexdocs.pm](https://hexdocs.pm/yaml/api-reference.html) oraz poszukać tutoriali i przykładowych projektów w Internecie.

## Zobacz także

- Dokumentacja biblioteki YAML na [hexdocs.pm](https://hexdocs.pm/yaml/api-reference.html)
- Przykładowy projekt korzystający z biblioteki YAML na [GitHub](https://github.com/elixir-yaml/yaml)
- Tutorial o pracy z YAML w Elixir na [Medium](https://medium.com/@petitviolet/yaml-in-elixir-b1c3e98f8aed)