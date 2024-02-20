---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:05.552077-07:00
description: "Trabalhar com JSON (JavaScript Object Notation) envolve analisar strings\
  \ formatadas em JSON para objetos Python e vice-versa. Isso \xE9 crucial para o\u2026"
lastmod: 2024-02-19 22:05:05.246664
model: gpt-4-0125-preview
summary: "Trabalhar com JSON (JavaScript Object Notation) envolve analisar strings\
  \ formatadas em JSON para objetos Python e vice-versa. Isso \xE9 crucial para o\u2026"
title: Trabalhando com JSON
---

{{< edit_this_page >}}

## O quê & Por quê?

Trabalhar com JSON (JavaScript Object Notation) envolve analisar strings formatadas em JSON para objetos Python e vice-versa. Isso é crucial para o desenvolvimento web e de API, pois o JSON é a língua franca para a troca de dados entre servidores e clientes.

## Como fazer:

A biblioteca integrada `json` do Python simplifica o processo de codificação (converter objetos Python para JSON) e decodificação (converter JSON para objetos Python). Veja como você pode usá-la:

### Codificando objetos Python para JSON:

```python
import json

dados = {
    "nome": "John Doe",
    "idade": 30,
    "eFuncionario": True,
    "enderecos": [
        {"cidade": "Nova York", "cep": "10001"},
        {"cidade": "São Francisco", "cep": "94016"}
    ]
}

json_string = json.dumps(dados, indent=4)
print(json_string)
```

**Saída:**

```json
{
    "nome": "John Doe",
    "idade": 30,
    "eFuncionario": true,
    "enderecos": [
        {
            "cidade": "Nova York",
            "cep": "10001"
        },
        {
            "cidade": "São Francisco",
            "cep": "94016"
        }
    ]
}
```

### Decodificando JSON para objetos Python:

```python
json_string = '''
{
    "nome": "John Doe",
    "idade": 30,
    "eFuncionario": true,
    "enderecos": [
        {
            "cidade": "Nova York",
            "cep": "10001"
        },
        {
            "cidade": "São Francisco",
            "cep": "94016"
        }
    ]
}
'''

dados = json.loads(json_string)
print(dados)
```

**Saída:**

```python
{
    'nome': 'John Doe', 
    'idade': 30, 
    'eFuncionario': True, 
    'enderecos': [
        {'cidade': 'Nova York', 'cep': '10001'}, 
        {'cidade': 'São Francisco', 'cep': '94016'}
    ]
}
```

### Trabalhando com bibliotecas de terceiros:

Para lidar com JSON de forma mais complexa, como validação de esquema ou análise de arquivos JSON diretamente de URLs, bibliotecas como `requests` para solicitações HTTP e `jsonschema` para validação podem ser úteis.

#### Exemplo com `requests` para analisar JSON de uma URL:

```python
import requests

resposta = requests.get('https://api.exemplo.com/dados')
dados = resposta.json()

print(dados)
```

Este trecho busca dados JSON de uma URL dada e os converte diretamente em um objeto Python.

#### Usando `jsonschema` para validar JSON:

Primeiro, instale a biblioteca via pip:

```bash
pip install jsonschema
```

Então, use-a da seguinte forma:

```python
from jsonschema import validate
import jsonschema

esquema = {
    "tipo": "object",
    "propriedades": {
        "nome": {"tipo": "string"},
        "idade": {"tipo": "number"},
        "eFuncionario": {"tipo": "boolean"},
    },
    "requeridos": ["nome", "idade", "eFuncionario"]
}

# Supondo que `dados` seja um dicionário obtido da decodificação de JSON
try:
    validate(instance=dados, schema=esquema)
    print("Dados JSON válidos.")
except jsonschema.exceptions.ValidationError as err:
    print("Erro de validação:", err)
```

Este exemplo valida seu dicionário Python (obtido de dados JSON decodificados) contra um esquema pré-definido, garantindo que os dados estejam em conformidade com os formatos e tipos esperados.
