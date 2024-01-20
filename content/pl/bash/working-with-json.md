---
title:                "Praca z json"
html_title:           "Bash: Praca z json"
simple_title:         "Praca z json"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/working-with-json.md"
---

{{< edit_this_page >}}

## O co chodzi i dlaczego?

JSON (JavaScript Object Notation) to popularny format danych używany przez programistów do przechowywania i przesyłania informacji w prosty i czytelny sposób. Jest to tekstowy format, który można wykorzystać w wielu językach programowania, w tym w Bash. Programiści lubią pracować z JSON, ponieważ jest łatwo przetwarzalny przez komputery oraz jest stosunkowo intuicyjny dla ludzi do czytania.

## Jak to zrobić?

Aby pracować z JSON w Bash, musisz użyć komendy `jq`, która jest narzędziem do analizowania i manipulowania JSON. Poniżej znajdują się przykładowe polecenia z wykorzystaniem `jq` oraz odpowiadające im wyjścia:

```Bash
# Wyświetlenie zawartości pliku JSON
$ jq '.' plik.json

{"imię": "Anna", "wiek": 30, "hobby": ["czytanie", "pływanie", "gotowanie"]}

# Wyświetlenie wartości pod kluczem "wiek"
$ jq '.wiek' plik.json

30

# Wyświetlenie wszystkich wartości z tablicy pod kluczem "hobby"
$ jq '.hobby[]' plik.json

czytanie
pływanie
gotowanie
```

## Głębsza analiza

JSON został opracowany w latach 90. jako format danych dla języka programowania JavaScript. Od tego czasu stał się powszechnie używany w innych językach programowania, w tym w Bash. W przeciwieństwie do innych formatów danych, JSON jest składniowo prosty i nie wymaga osobnego programu do odczytu i zapisu danych. Alternatywami dla JSON są m.in. YAML, XML czy CSV, jednakże JSON jest najczęściej wybieranym formatem ze względu na swoją prostotę i czytelność.

W Bash, `jq` wykorzystuje język wyrażeń regularnych oraz składniowe elementy języka JavaScript do przetwarzania danych w formacie JSON. Aby uzyskać pełną listę dostępnych funkcji `jq`, można skorzystać z dokumentacji tego narzędzia.

## Zobacz też

- [Dokumentacja oficjalna `jq`](https://stedolan.github.io/jq/)