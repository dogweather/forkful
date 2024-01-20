---
title:                "Trabalhando com json"
html_title:           "Python: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/working-with-json.md"
---

{{< edit_this_page >}}

## O que & Porquê?

Trabalhar com JSON é uma forma de processar e armazenar dados em um formato leve e fácil de ler e escrever. Programadores frequentemente utilizam JSON para compartilhar informações entre diferentes sistemas ou linguagens de programação.

## Como fazer:

```
# Importar a biblioteca json
import json

# Criar um objeto JSON
object = {'nome': 'Maria', 'idade': 25}

# Converter para string JSON
json_string = json.dumps(object)

# Imprimir a string JSON
print(json_string)

# Output: {"nome": "Maria", "idade": 25}
```

## Profundando:

JSON, sigla para JavaScript Object Notation, foi criado em 2001 por Douglas Crockford como um formato de intercâmbio de dados para ser utilizado em aplicações web. Ele é uma alternativa mais simples e leve ao formato XML e é suportado por diversas linguagens de programação. Em Python, a biblioteca padrão json oferece várias funções para trabalhar com JSON, como a conversão de objetos Python para JSON e vice-versa.

## Veja também:

- [Documentação Python sobre JSON](https://docs.python.org/3/library/json.html)
- [JSON Formatter & Validator](https://jsonformatter.curiousconcept.com/)