---
title:                "Praca z TOML"
date:                  2024-01-26T04:21:59.595882-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/working-with-toml.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
TOML to format pliku konfiguracyjnego, łatwy do odczytu i zapisu przez ludzi, oraz łatwy do przetwarzania i generowania przez maszyny. Programiści używają TOML dla jasnych, hierarchicznych plików konfiguracyjnych w projektach, gdzie czytelność jest kluczowa.

## Jak to zrobić:
Aby odczytać i manipulować TOML w Fish, można używać narzędzia takiego jak `yj`, które może konwertować TOML na JSON. Oto jak:

```fish
# Zainstaluj yj za pomocą Fishera
fisher install jorgebucaran/yj

# Konwertuj TOML na JSON
echo 'title = "Przykład TOML"' | yj -tj

# Przykładowy wynik
{"title":"Przykład TOML"}
```

Aby zapisać TOML, proces jest odwracany:

```fish
# Konwertuj JSON na TOML
echo '{"title":"Przykład JSON"}' | yj -jt

# Przykładowy wynik
title = "Przykład JSON"
```

Do cięższych zadań warto rozważyć dedykowane narzędzie CLI dla TOML, jak `toml-cli`.

```fish
# Zainstaluj toml-cli
pip install toml-cli

# Ustaw wartość w pliku TOML
toml set pyproject.toml tool.poetry.version "1.1.4"

# Pobierz wartość z pliku TOML
set version (toml get pyproject.toml tool.poetry.version)
echo $version
```

## Szczegółowa analiza
TOML (Tom's Obvious, Minimal Language), wprowadzony przez Toma Preston-Wernera w 2013 roku, jest podobny do INI, ale z zdefiniowaną specyfikacją i hierarchią danych. JSON i YAML to główne alternatywy, ale mają swoje kompromisy: JSON nie jest tak przyjazny dla użytkownika, podczas gdy YAML jest bardziej skomplikowany. Projekt TOML odnosi sukcesy w scenariuszach, gdzie pliki konfiguracyjne są często utrzymywane ręcznie, równoważąc prostotę i wyrazistość. Jeśli chodzi o implementację, parsery TOML są dostępne dla większości języków programowania, w tym TomlBombadil dla Fish, który może być łatwo włączony do Twoich skryptów.

## Zobacz również
- Oficjalna specyfikacja TOML: https://toml.io
- `yj`, narzędzie do konwersji między TOML, JSON, YAML i XML: https://github.com/jorgebucaran/yj
- `toml-cli`, narzędzie linii poleceń dla TOML: https://github.com/sdispater/toml-cli