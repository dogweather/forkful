---
title:                "Trabalhando com YAML"
date:                  2024-01-19
html_title:           "Arduino: Trabalhando com YAML"
simple_title:         "Trabalhando com YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## O Que é e Por Quê?

Trabalhar com YAML significa manipular um formato de dados que é humano amigável, usado para configuração de aplicações. Programadores usam YAML porque ele permite uma leitura fácil e é amplamente aceito em várias linguagens de programação, incluindo Python.

## Como Fazer:

```python
import yaml

# Carregando dados de um arquivo YAML
with open('config.yaml', 'r') as file:
    config = yaml.safe_load(file)
print(config)

# Salvando dados em um arquivo YAML
data = {'database': {'user': 'admin', 'password': 'secret'}}
with open('output.yaml', 'w') as file:
    yaml.dump(data, file)

# Saída esperada para o carregamento de dados:
# Supondo que 'config.yaml' contém:
# database:
#   user: admin
#   password: secret
#
# A saída será:
# {'database': {'user': 'admin', 'password': 'secret'}}
```

## Mergulho Profundo

YAML começou em 2001 como uma alternativa ao XML para facilitar a serialização de dados. Enquanto JSON é uma alternativa mais simples, YAML destaca-se na legibilidade e suporte para comentários. Internamente, ao trabalhar com YAML em Python, a biblioteca `PyYAML` é frequentemente utilizada, provedora dos métodos `load` e `dump`.

## Veja Também

- Documentação oficial do PyYAML: https://pyyaml.org/wiki/PyYAMLDocumentation
- YAML 1.2 especificação: https://yaml.org/spec/1.2/spec.html
- Uma comparação entre YAML, JSON e XML: https://stackoverflow.com/questions/1726802/what-is-the-difference-between-yaml-and-json-when-to-prefer-one-over-the-other
