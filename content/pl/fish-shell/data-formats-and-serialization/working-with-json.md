---
title:                "Praca z JSON"
aliases: - /pl/fish-shell/working-with-json.md
date:                  2024-02-03T19:22:36.192561-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Praca z JSON w Fish Shell polega na analizowaniu i generowaniu danych JSON, co jest powszechnym zadaniem podczas konfiguracji aplikacji, interakcji z API oraz usprawniania przepływów pracy w wierszu poleceń. Biorąc pod uwagę wszechobecność JSON w rozwoju stron internetowych i aplikacji, opanowanie manipulacji nim bezpośrednio w powłoce może znacząco zwiększyć efektywność automatyzacji i obsługi danych dla programistów.

## Jak to zrobić:

Sam Fish Shell nie posiada wbudowanych narzędzi do analizowania i generowania JSON. Jednakże, bezproblemowo integruje się z narzędziami firm trzecich takimi jak `jq` do przetwarzania JSON. `jq` jest potężnym i wszechstronnym procesorem JSON-a działającym w wierszu poleceń, który pozwala ci kroić, filtrować, mapować i transformować strukturalne dane przy pomocy prostego i ekspresyjnego języka.

### Analizowanie JSON z jq
Aby przeanalizować plik JSON i wyodrębnić dane za pomocą `jq`:

```fish
# Przyjmując, że masz plik JSON o nazwie 'data.json' z zawartością: {"name":"Fish Shell","version":"3.4.0"}
cat data.json | jq '.name'
# Przykładowe wyjście
"Fish Shell"
```

### Generowanie JSON z jq
Tworzenie treści JSON z zmiennych w powłoce lub wyników:

```fish
# Utwórz obiekt JSON z zmiennych
set name "Fish Shell"
set version "3.4.0"
jq -n --arg name "$name" --arg version "$version" '{name: $name, version: $version}'
# Przykładowe wyjście
{
  "name": "Fish Shell",
  "version": "3.4.0"
}
```

### Filtrowanie kolekcji JSON
Załóżmy, że mamy tablicę obiektów JSON w pliku o nazwie `versions.json`:
```json
[
  {"version": "3.1.2", "stable": true},
  {"version": "3.2.0", "stable": false},
  {"version": "3.4.0", "stable": true}
]
```
Aby przefiltrować tę tablicę tylko pod kątem stabilnych wersji:

```fish
cat versions.json | jq '.[] | select(.stable == true) | .version'
# Przykładowe wyjście
"3.1.2"
"3.4.0"
```

Przykłady te pokazują moc integracji `jq` z Fish Shell do operacji na JSON. Wykorzystanie takich narzędzi wzbogaca doświadczenie z korzystania z powłoki, czyniąc ją potężnym środowiskiem do obsługi nowoczesnych formatów danych.
