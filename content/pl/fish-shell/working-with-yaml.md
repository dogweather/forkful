---
title:                "Praca z formatem YAML"
html_title:           "Fish Shell: Praca z formatem YAML"
simple_title:         "Praca z formatem YAML"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Co to jest i po co się to robi?

Working with YAML (Yet Another Markup Language) in programming is a way to organize and store data in a human-readable format. It is often used for configuring applications, creating data structures, and transferring data between systems. Programmers use YAML because it is easy to read and write, supports data types and structures, and integrates well with other programming languages and tools.

## Jak to zrobić:

### Przykład 1: Tworzenie pliku YAML

```Fish Shell
echo 'name: John
age: 30
hobbies:
  - hiking
  - cooking
' > person.yml
```
W wyniku powyższego kodu zostanie utworzony plik `person.yml` z danymi osobowymi osoby o imieniu John, wieku 30 lat i dwóch zainteresowaniach.

### Przykład 2: Odczytywanie pliku YAML

```Fish Shell
set -q person.name <(yq e '.name' person.yml)
echo $person_name
```
W wyniku powyższego kodu zostanie wczytana wartość `name` z pliku `person.yml` i przypisana do zmiennej `person_name`.

## Głębsza analiza:

### Kontekst historyczny

YAML został stworzony w 2001 roku przez Clarka Evansa i Ingy'ego dot Net jako alternatywna składnia dla XML. Jego głównym celem było dostarczenie prostego i czytelnego sposobu na przechowywanie i przetwarzanie danych strukturalnych.

### Alternatywy

YAML jest jednym z wielu formatów do przechowywania danych i konfiguracji. Alternatywami dla YAML są między innymi JSON i XML. Każdy z tych formatów ma swoje zalety i w zależności od potrzeb projektu, programiści mogą wybierać między nimi.

### Szczegóły implementacji

Fish Shell posiada wbudowane polecenia do obsługi plików YAML, takie jak `yq` czy `jq`. Można również zainstalować dodatkowe pluginy, które ułatwią pracę z tym formatem. Dzięki temu programiści mogą szybko i wygodnie zarządzać danymi YAML bez potrzeby korzystania z zewnętrznych narzędzi.

## Zobacz też:

- [Dokumentacja Fish Shell dotycząca YAML](https://fishshell.com/docs/current/tutorial.html#yaml-support)
- [Strona domowa YAML](https://yaml.org/)