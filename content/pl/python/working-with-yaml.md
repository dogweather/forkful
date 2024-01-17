---
title:                "Praca z yaml"
html_title:           "Python: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/working-with-yaml.md"
---

{{< edit_this_page >}}

# Co & Dlaczego?

Praca z YAML to niezbędna umiejętność dla każdego programisty, ponieważ ten format jest szeroko stosowany do przechowywania danych konfiguracyjnych i strukturalnych. Jest czytelny dla ludzi i maszyn, co czyni go wygodnym do użycia w wielu różnych kontekstach.

# Jak to zrobić:

Zaimportuj moduł `yaml` aby móc używać funkcji do parsowania i generowania plików YAML.

```Python
import yaml

# Parsowanie YAML do słownika Python
with open("example.yaml", "r") as file:
    data = yaml.load(file, Loader=yaml.FullLoader)

# Generowanie pliku YAML z słownika Python
info = {'name': 'Jan Kowalski', 'age': 30}
with open("info.yaml", "w") as file:
    yaml.dump(info, file)
```

# Głębsza analiza:

1. Kontekst historyczny: YAML został stworzony w 2001 roku jako uproszczony sposób na definiowanie danych strukturalnych. Jego nazwa to akronim "YAML Ain't Markup Language".
2. Alternatywy: Alternatywami dla YAML są Facebook's YML i XML. Jednak YAML jest często uważany za łatwiejszy w użyciu i bardziej czytelny.
3. Szczegóły implementacyjne: Struktura danych w YAML jest oparta na tablicach, słownikach i listach. Dane mogą być również zapisane za pomocą mapowania sekweneyjnego.

# Zobacz także:

Sprawdź oficjalną dokumentację YAML dla pełnej listy funkcji oraz przykłady zastosowań:
https://pyyaml.org/wiki/PyYAMLDocumentation

Dowiedz się więcej na temat różnych formatów danych, które są wykorzystywane w programowaniu:
https://www.w3schools.com/xml/xml_whatis.asp