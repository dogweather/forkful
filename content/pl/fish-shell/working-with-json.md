---
title:                "Praca z json"
html_title:           "Fish Shell: Praca z json"
simple_title:         "Praca z json"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## Co & dlaczego?
JSON jest formatem danych często używanym przez programistów do przechowywania i przesyłania danych w postaci tekstu. Umożliwia łatwe przechowywanie i odczytywanie danych w sposób zrozumiały dla komputerów, co ułatwia pracę z dużymi zbiorami danych.

## Jak to zrobić:
W programowaniu z użyciem Fish Shell możemy łatwo pracować z danymi w formacie JSON przy użyciu wbudowanych funkcji i poleceń. Możemy użyć komendy ```jq``` aby filtrować dane i wybierać konkretne pola, a następnie wyświetlić wynik w wybranym formacie, na przykład:

```
$ cat data.json | jq '.name'
"John Smith"
```

Jeśli chcemy zmienić format danych, możemy użyć komendy ```json2yaml``` aby przekonwertować dane z JSON na YAML lub ```yaml2json``` aby dokonać odwrotnej konwersji.

## Deep Dive:
JSON został stworzony w 2001 roku przez Douglasa Crockforda i szybko stał się jednym z najpopularniejszych formatów przechowywania danych. Alternatywami dla JSON są na przykład XML lub CSV, jednak JSON jest popularniejszy ze względu na swoją prostotę i czytelność.

Implementacja obsługi JSON w Fish Shell jest oparta na bibliotece ```libyajl```, która zapewnia wydajne i bezpieczne przetwarzanie danych w formacie JSON.

## Zobacz także:
Dokumentacja Fish Shell (https://fishshell.com/docs/current/cmds.html)
Dokumentacja biblioteki ```libyajl``` (https://lloyd.github.io/yajl/)