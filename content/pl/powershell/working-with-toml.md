---
title:                "Praca z TOML"
date:                  2024-01-26T04:25:14.981918-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z TOML"

category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/working-with-toml.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

TOML, skrót od Tom's Obvious, Minimal Language, to format serializacji danych, który jest łatwy do odczytania dzięki swojej jasnej semantyce. Programiści używają go do plików konfiguracyjnych, ponieważ znajduje równowagę między czytelnością dla człowieka a przyjaznością dla maszyn.

## Jak to zrobić:

W PowerShellu nie ma natywnej cmdlet do parsowania TOML. Zwykle używa się modułu lub konwertuje TOML na JSON za pomocą narzędzia takiego jak `toml-to-json`, jeśli chcesz pracować z PowerShell. Oto jak zrobić to z użyciem fikcyjnego modułu `PowerShellTOML`:

```PowerShell
# Najpierw zainstaluj moduł (wymyślony, dla demonstracji)
Install-Module PowerShellTOML

# Importuj plik TOML
$config = Import-TomlConfig -Path './config.toml'

# Dostęp do wartości
Write-Output $config.database.server

# Przykładowa zawartość TOML w 'config.toml':
# [database]
# server = "192.168.1.1"
# ports = [ 8001, 8001, 8002 ]
# connection_max = 5000

# Przykładowe wyjście:
# 192.168.1.1
```

## Szczegółowe omówienie

TOML został stworzony przez Toma Preston-Wernera, współzałożyciela GitHuba, jako prostsza alternatywa dla XML i YAML dla plików konfiguracyjnych. Jego pierwsza wersja pojawiła się w 2013 roku. TOML jest porównywalny do JSON, ale jest zaprojektowany, aby być bardziej przyjazny dla człowieka, co czyni go dobrym wyborem dla konfiguracji, która jest utrzymywana przez ludzi. Alternatywy obejmują YAML, JSON i XML.

Pod względem implementacji, moduł PowerShell dla TOML byłby zwykle opakowaniem wokół biblioteki TOML napisanej w bardziej wydajnym języku, takim jak C#. PowerShell nie ma wbudowanego wsparcia dla TOML, dlatego taki moduł jest konieczny, aby wygodnie współpracować z formatem TOML.

## Zobacz również

- Standard TOML: https://toml.io/en/
- Repozytorium GitHub dla modułu `toml` PowerShell (jeśli istnieje w momencie czytania): https://github.com/powershell/PowerShellTOML
- Wprowadzenie do TOML: https://github.com/toml-lang/toml
- Porównanie formatów serializacji danych: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
