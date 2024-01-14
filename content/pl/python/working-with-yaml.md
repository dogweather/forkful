---
title:                "Python: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego

Python jest jednym z najpopularniejszych języków programowania na świecie, a YAML (Yet Another Markup Language) jest jednym z wielu formatów używanych w programowaniu. Kombinacja tych dwóch narzędzi może umożliwić programistom pracę z danymi i konfiguracją w sposób bardziej czytelny i intuicyjny. W tym artykule dowiesz się, dlaczego warto zainteresować się pracą z YAML w Pythonie.

## Jak zacząć

Aby rozpocząć pracę z YAML w Pythonie, pierwszą rzeczą, którą musisz zrobić, to zainstalować paczkę pyyaml. W tym celu możesz użyć pip, czyli menedżera pakietów dla języka Python:

```Python
pip install pyyaml
```

Po zainstalowaniu pyyaml, możesz zacząć korzystać z jego funkcjonalności. Najważniejszą częścią pracy z YAML w Pythonie jest umiejętne przetwarzanie danych w tym formacie. Poniżej przedstawiono prosty przykład wykorzystania pyyaml:

```Python
import yaml

# Przykładowe dane w formacie YAML
data = """
fruits:
  - apple
  - orange
  - banana
vegetables:
  - carrot
  - tomato
  - cucumber
"""

# Parsowanie danych YAML do słownika
parsed_data = yaml.safe_load(data)

# Wyświetlenie zawartości słownika
print(parsed_data)

# Wynik:
# {'fruits': ['apple', 'orange', 'banana'], 'vegetables': ['carrot', 'tomato', 'cucumber']}
```

Jak widać, po zaimportowaniu modułu yaml i wykorzystaniu funkcji safe_load, dane w formacie YAML są przetwarzane do postaci słownika Pythonowego, co umożliwia łatwe korzystanie z tych danych w dalszej części kodu.

## Deep Dive

Dzięki wykorzystaniu pakietu pyyaml, możesz również utworzyć plik YAML za pomocą kodu Pythona, a także łatwo modyfikować już istniejące pliki tego typu. Poniżej przedstawiono przykładowy kod, który tworzy plik YAML i zapisuje go na dysku:

```Python
import yaml

# Przygotowanie danych do zapisania w formacie YAML
config = {
  'server': 'localhost',
  'port': 8080,
  'database': {
    'name': 'example_db',
    'user': 'admin',
    'password': 'admin123'
  }
}

# Zapisanie danych do pliku YAML
with open('config.yaml', 'w') as f:
    yaml.dump(config, f)

# Wynik: plik config.yaml z zawartością:
# server: localhost
# port: 8080
# database:
#   name: example_db
#   user: admin
#   password: admin123
```

Powyższy kod może być również wykorzystany do modyfikacji już istniejących plików YAML. Wystarczy zmienić wartości w słowniku config i ponownie wywołać funkcję dump.

Podsumowując, wykorzystanie YAML w Pythonie może ułatwić pracę z danymi i konfiguracją, szczególnie w przypadku dużej ilości informacji. Ponadto, dzięki pakietowi pyyaml, możesz łatwo przetwarzać i tworzyć pliki tego typu za pomocą swojego kodu.

## Zobacz również

- [Dokumentacja modułu pyyaml](https://pyyaml.org/wiki/PyYAMLDocumentation)
- [Paczka pyyaml na stronie PyPI](https://pypi.org/project/PyYAML/)
- [Oficjalna strona języka YAML](https://yaml.org/)