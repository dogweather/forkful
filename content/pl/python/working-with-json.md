---
title:                "Praca z JSON"
date:                  2024-01-19
html_title:           "Bash: Praca z JSON"
simple_title:         "Praca z JSON"

category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
Praca z JSON to zarządzanie danymi w popularnym formacie - kluczowym dla API i konfiguracji. Programiści używają JSON, bo jest lekki, czytelny i szeroko wspierany.

## How to:
```Python
import json

# Zapisywanie danych do JSON
dane = {'imie': 'Jan', 'wiek': 30}
with open('przyklad.json', 'w') as plik:
    json.dump(dane, plik)

# Odczytywanie danych z JSON
with open('przyklad.json', 'r') as plik:
    przeczytane_dane = json.load(plik)
print(przeczytane_dane)
```
Output:
```
{'imie': 'Jan', 'wiek': 30}
```

## Deep Dive
JSON, czyli JavaScript Object Notation, narodził się z JavaScriptu, ale zdobył niezależność. Alternatywami są XML (cięższy), YAML (łatwiejszy dla człowieka do zrozumienia) czy CSV (prostszy). Python wykorzystuje moduł `json` do pracy z tym formatem, który mapuje format JSON bezpośrednio na słowniki i listy.

## See Also
- [Oficjalna dokumentacja Python dla modułu json](https://docs.python.org/3/library/json.html)
- [Dokumentacja JSON](https://www.json.org/json-pl.html)
- [Porównanie JSON i XML](https://www.w3schools.com/js/js_json_xml.asp)
