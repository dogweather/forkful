---
date: 2024-01-26 04:25:38.744987-07:00
description: "TOML, skr\xF3t od Tom's Obvious, Minimal Language, to format serializacji\
  \ danych podobny do JSON lub YAML, ale maj\u0105cy na celu prostot\u0119 i czytelno\u015B\
  \u0107.\u2026"
lastmod: '2024-03-13T22:44:34.974704-06:00'
model: gpt-4-0125-preview
summary: "TOML, skr\xF3t od Tom's Obvious, Minimal Language, to format serializacji\
  \ danych podobny do JSON lub YAML, ale maj\u0105cy na celu prostot\u0119 i czytelno\u015B\
  \u0107."
title: Praca z TOML
weight: 39
---

## Co i dlaczego?
TOML, skrót od Tom's Obvious, Minimal Language, to format serializacji danych podobny do JSON lub YAML, ale mający na celu prostotę i czytelność. Programiści używają TOML do plików konfiguracyjnych, ponieważ jest łatwy do napisania i zrozumienia, a także ładnie mapuje się na struktury danych w językach programowania, takich jak Python.

## Jak to zrobić?
Przed rozpoczęciem, zainstaluj pakiet `toml` za pomocą polecenia `pip install toml`. Przeanalizujmy plik TOML:

```python
import toml

# Przykładowa zawartość TOML jako ciąg znaków
toml_string = """
[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z # Daty pierwszej klasy

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
"""

# Parsowanie ciągu TOML
parsed_toml = toml.loads(toml_string)

# Dostęp do danych
print(parsed_toml['owner']['name'])  # Wynik: Tom Preston-Werner
print(parsed_toml['database']['ports'])  # Wynik: [8001, 8001, 8002]
```

## Wgłębienie się
TOML został stworzony przez Toma Preston-Wernera, jednego z założycieli GitHub, jako bardziej przyjazny format plików konfiguracyjnych. Jest zaprojektowany tak, aby jednoznacznie mapować się na tabelę hasz i być łatwo analizowalny przez maszyny.

W porównaniu do JSON, TOML jest bardziej czytelny dla plików konfiguracyjnych i obsługuje komentarze. YAML, inna alternatywa, może być bardziej zwarty, ale jego zależność od wcięć i subtelne kwestie, takie jak brak wsparcia dla tabulatorów, mogą sprawiać trudności.

Jeśli chodzi o szczegóły implementacji, wartości TOML są typizowane, co obejmuje ciągi znaków, liczby całkowite, liczby zmiennoprzecinkowe, wartości logiczne, daty i czas, tablice oraz tabele. Wszystko jest uwzględniane z uwagą na wielkość liter. Ponadto TOML obsługuje ciągi wieloliniowe i, w najnowszej wersji, nawet pozwala na tablice o heterogenicznych typach.

Python używa biblioteki `toml`, która odzwierciedla biblioteki JSON i YAML pod względem API. Masz do dyspozycji `toml.load` i `toml.loads` do odczytu TOML z pliku lub ciągu znaków, odpowiednio, oraz `toml.dump` i `toml.dumps` do ich zapisu.

## Zobacz również
- Oficjalne repozytorium GitHub TOML ze specyfikacjami: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- Dokumentacja biblioteki `toml` Pythona: [pypi.org/project/toml/](https://pypi.org/project/toml/)
- Przykłady użycia TOML w realnym świecie: pliki konfiguracyjne menedżera pakietów Rusta `cargo` lub narzędzia do pakowania Pythona `poetry`.
