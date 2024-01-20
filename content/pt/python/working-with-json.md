---
title:                "Trabalhando com JSON"
html_title:           "Arduino: Trabalhando com JSON"
simple_title:         "Trabalhando com JSON"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/working-with-json.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?

JSON (JavaScript Object Notation) é um formato de troca de dados leve e fácil de ler para humanos. Programadores usam JSON para armazenar e transmitir dados entre servidores e clientes web e para configurações de aplicações devido à sua simplicidade e interoperabilidade.

## Como Fazer:

```Python
import json

# Criando um dicionário Python e convertendo para uma string JSON
dados = {"nome": "João", "idade": 30, "programador": True}
json_str = json.dumps(dados)
print(json_str)

# Convertendo string JSON de volta para um dicionário Python
dados_parsed = json.loads(json_str)
print(dados_parsed)
```

Saída:
```
{"nome": "João", "idade": 30, "programador": true}
{'nome': 'João', 'idade': 30, 'programador': True}
```

Manuseio de arquivos JSON:

```Python
# Escrevendo JSON para um arquivo
with open('dados.json', 'w') as f:
    json.dump(dados, f)

# Lendo JSON a partir de um arquivo
with open('dados.json', 'r') as f:
    dados_carregados = json.load(f)
    print(dados_carregados)
```

Saída:
```
{'nome': 'João', 'idade': 30, 'programador': True}
```

## Aprofundamento:

- *Contexto histórico*: JSON foi proposto por Douglas Crockford em 2001, buscando uma alternativa mais enxuta ao XML.
- *Alternativas*: Além do JSON, existem outros formatos como XML, YAML e BSON para troca de dados.
- *Detalhes de implementação*: A biblioteca json em Python segue o padrão RFC 7159 e é construída em C, o que a torna bastante eficiente.

## Veja Também:

- Documentação oficial do Python para JSON: https://docs.python.org/3/library/json.html
- Comparação entre JSON e XML: https://www.json.org/json-pt.html
- RFC 7159 – The JavaScript Object Notation (JSON) Data Interchange Format: https://tools.ietf.org/html/rfc7159