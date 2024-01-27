---
title:                "Praca z yaml"
date:                  2024-01-19
html_title:           "Arduino: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
YAML to język służący do reprezentacji danych, popularny w konfiguracjach. Programiści korzystają z niego, bo jest czytelny dla człowieka i łatwy w użyciu.

## Jak to zrobić:
Aby pracować z YAML w Pythonie, użyjemy biblioteki PyYAML. Najpierw zainstaluj ją używając `pip`:

```bash
pip install PyYAML
```

Następnie możemy ładować i zapisywać YAML tak:

```Python
import yaml

# Ładowanie YAML z pliku
with open('example.yaml', 'r') as file:
    data = yaml.safe_load(file)

# Praca z danych YAML
print(data)

# Zapis do pliku YAML
new_data = {'key': 'value'}
with open('new_example.yaml', 'w') as file:
    yaml.dump(new_data, file)
```

Testowy plik `example.yaml` mógłby wyglądać tak:
```yaml
key: value
list:
  - item1
  - item2
```

A output z kodu Python:
```Python
{'key': 'value', 'list': ['item1', 'item2']}
```

## Głębiej:
YAML powstał w 2001 roku jako bardziej czytelna alternatywa dla XML i JSON. Chociaż JSON jest często używanym formatem w API i konfiguracjach, YAML jest popularny w narzędziach takich jak Docker, Kubernetes czy Ansible ze względu na swoją przejrzystość. PyYAML jest jedną z kilku bibliotek dostępnych w Pythonie, istnieją też ruamel.yaml (częściej aktualizowany) i oyaml (zapewnia zachowanie kolejności).

## Zobacz również:
- Dokumentacja PyYAML: https://pyyaml.org/wiki/PyYAMLDocumentation
- Specyfikacja YAML: https://yaml.org/spec/1.2/spec.html
- Porównanie YAML i JSON: https://json2yaml.com/
