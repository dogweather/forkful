---
title:                "Praca z yaml"
date:                  2024-01-19
html_title:           "Arduino: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
YAML to format przechowywania danych, który jest prosty dla ludzi i komputerów. Programiści używają YAML, bo jest czytelny i łatwo się z nim pracuje, zwłaszcza przy konfiguracji aplikacji i przechowywaniu prostych danych.

## Jak to zrobić:
Ruby ma wbudowane wsparcie dla YAML przez bibliotekę 'yaml'. Oto jak to działa:

```Ruby
require 'yaml'

# Zapiszmy do pliku YAML
data = {"name" => "Mateusz", "profession" => "Programista"}
File.open("user.yml", "w") { |file| file.write(data.to_yaml) }

# Teraz odczytajmy z pliku YAML
yaml_content = File.read("user.yml")
loaded_data = YAML.load(yaml_content)
puts loaded_data['name'] # Wyświetli 'Mateusz'
```

## Deep Dive
YAML (YAML Ain't Markup Language) powstał w 2001 roku. Alternatywą dla YAML jest JSON, który jest bardziej skomplikowany w odczycie dla ludzi, ale często szybszy w przetwarzaniu przez maszyny. YAML używa wcięć do reprezentacji hierarchii i obsługuje różne typy danych, co czyni go elastycznym wyborem do różnorodnych zastosowań.

## Zobacz też:
- Oficjalna strona YAML: https://yaml.org
- RubyDoc dla biblioteki YAML: https://ruby-doc.org/stdlib-2.5.1/libdoc/yaml/rdoc/YAML.html
- Porównanie YAML i JSON: https://hackernoon.com/json-vs-yaml-a0123b86a41b
