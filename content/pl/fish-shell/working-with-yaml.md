---
title:                "Praca z yaml"
date:                  2024-01-19
simple_title:         "Praca z yaml"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML to format zapisu danych czytelny dla ludzi - świetnie nadaje się do konfiguracji. Programiści używają go, by łatwiej zarządzać ustawieniami aplikacji i środowisk.

## How to:
### Przykładowe dane YAML
```yaml
# config.yaml
nazwa: "Przykładowa Aplikacja"
port: 8080
ścieżki:
  - /api
  - /home
```
### Czytanie z pliku YAML w Fish
```fish
set data (yaml read config.yaml)
echo $data[nazwa] # Wyświetla "Przykładowa Aplikacja"
```
### Zapis do pliku YAML w Fish
```fish
yaml write config.yaml nazwa "Nowa Aplikacja"
# Zaktualizuje nazwę w pliku config.yaml
```
### Instalacja narzędzia do obsługi YAML
```fish
fisher install jorgebucaran/reed
# Instaluje plugin `yaml` do Fish Shell
```

## Deep Dive
YAML narodził się w 2001 roku jako alternatywa dla XML i JSON, stawiając na prostotę i czytelność. W Fish Shell nie ma wbudowanej obsługi YAML, używamy więc zewnętrznych narzędzi jak `yaml` od Jorge Bucaran lub `shyaml`. Warto pamiętać, że YAML jest wrażliwy na formatowanie, szczególnie na wcięcia.

## See Also
- [Oficjalna strona YAML](https://yaml.org/)
- [Dokumentacja Fish Shell](https://fishshell.com/docs/current/index.html)
- [Dokumentacja 'shyaml'](https://github.com/0k/shyaml)
