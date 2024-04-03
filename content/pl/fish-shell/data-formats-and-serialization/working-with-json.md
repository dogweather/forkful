---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:36.192561-07:00
description: "Jak to zrobi\u0107: Sam Fish Shell nie posiada wbudowanych narz\u0119\
  dzi do analizowania i generowania JSON. Jednak\u017Ce, bezproblemowo integruje si\u0119\
  \ z narz\u0119dziami\u2026"
lastmod: '2024-03-13T22:44:35.863115-06:00'
model: gpt-4-0125-preview
summary: "Sam Fish Shell nie posiada wbudowanych narz\u0119dzi do analizowania i generowania\
  \ JSON."
title: Praca z JSON
weight: 38
---

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
