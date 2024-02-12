---
title:                "Praca z YAML"
aliases: - /pl/python/working-with-yaml.md
date:                  2024-02-03T19:26:38.060045-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
YAML, czyli YAML Ain't Markup Language, to format serializacji danych czytelny dla człowieka. Programiści używają YAML dla plików konfiguracyjnych, międzyprocesowej komunikacji i przechowywania danych ze względu na jego prostą składnię i łatwość odczytu w porównaniu do innych formatów, takich jak XML czy JSON.

## Jak to zrobić:
Odczyt i zapis w YAML w Pythonie zazwyczaj wymaga użycia zewnętrznej biblioteki, przy czym `PyYAML` jest najbardziej popularny. Aby rozpocząć, musisz zainstalować PyYAML, wykonując `pip install PyYAML`.

**Przykład: Zapis do pliku YAML**

```python
import yaml

data = {'a list': [1, 42, 3.141, 1337, 'help', u'€'],
        'a string': 'boo!',
        'another dict': {'foo': 'bar', 'key': 'value', 'the answer': 42}}

with open('example.yaml', 'w') as f:
    yaml.dump(data, f, default_flow_style=False)

# To tworzy `example.yaml` z danymi zorganizowanymi w formacie YAML.
```

**Przykład: Odczyt z pliku YAML**

```python
import yaml

with open('example.yaml', 'r') as f:
    data_loaded = yaml.safe_load(f)

print(data_loaded)

# Wynik: 
# {'a list': [1, 42, 3.141, 1337, 'help', '€'],
#  'a string': 'boo!',
#  'another dict': {'foo': 'bar', 'key': 'value', 'the answer': 42}}
```

**Używanie YAML do konfiguracji**

Wielu programistów używa YAML do zarządzania konfiguracjami aplikacji. Oto przykład, jak można zorganizować plik konfiguracyjny i odczytać go:

config.yaml:
```yaml
database:
  host: localhost
  port: 5432
  username: admin
  password: secret
```

Odczytywanie pliku konfiguracyjnego w Pythonie:
```python
import yaml

with open('config.yaml', 'r') as f:
    config = yaml.safe_load(f)

print(config['database']['host'])  # Wynik: localhost
```

**Obsługa złożonych struktur**

Dla złożonych struktur, PyYAML pozwala definiować niestandardowe obiekty Pythona. Jednak, dla bezpieczeństwa, należy używać `safe_load`, aby unikać wykonywania arbitralnych funkcji lub obiektów.

```python
import yaml

# Definiowanie obiektu Pythona
class Example:
    def __init__(self, value):
        self.value = value

# Niestandardowy konstruktor
def constructor_example(loader, node):
    value = loader.construct_scalar(node)
    return Example(value)

# Dodawanie konstruktora dla tagu "!example"
yaml.add_constructor('!example', constructor_example)

yaml_str = "!example 'data'"
loaded = yaml.load(yaml_str, Loader=yaml.FullLoader)

print(loaded.value)  # Wynik: data
```

W tym fragmencie, `!example` to niestandardowy tag używany do instancji obiektu `Example` z wartością 'data' z ciągu YAML. Niestandardowe ładowarki takie jak ta zwiększają elastyczność PyYAML, umożliwiając przetwarzanie bardziej złożonych struktur danych i typów.
