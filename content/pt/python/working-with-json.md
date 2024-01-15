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

## Por que trabalhar com JSON?

JSON (JavaScript Object Notation) é um formato de dados amplamente utilizado para armazenar e transmitir informações em uma estrutura de texto simples. Ele é comumente usado em aplicações web e móveis, pois é leve, fácil de ler e manipular por máquinas e humanos. Trabalhar com JSON pode melhorar a eficiência e facilidade de comunicação em projetos de programação.

## Como fazer?

### Criando um objeto JSON

Para começar a trabalhar com JSON em Python, é necessário primeiro importar o módulo `json`.

 ```Python
 import json
 ```

Então, podemos criar um objeto JSON básico usando a função `dumps()` para converter um dicionário Python em uma string JSON:

```Python
carro = {
  "marca": "Tesla",
  "modelo": "Model 3",
  "ano": 2020,
  "cor": "branco"
}

json_carro = json.dumps(carro)
print(json_carro)
```

Isso produzirá a seguinte saída:

```
{"marca": "Tesla", "modelo": "Model 3", "ano": 2020, "cor": "branco"}
```

### Codificando e decodificando JSON

Além de criar objetos JSON, também podemos codificar e decodificar strings JSON em Python usando as funções `dumps()` e `loads()` respectivamente.

```Python
# Codificando
mensagem = "Olá, mundo!"
json_mensagem = json.dumps(mensagem)
print(json_mensagem)

# Decodificando
json_mensagem = '{"mensagem": "Olá, mundo!"}'
mensagem = json.loads(json_mensagem)
print(mensagem["mensagem"])
```

Isso produzirá a seguinte saída:

```
"Olá, mundo!"
Olá, mundo!
```

### Salvando e carregando arquivos JSON

Podemos salvar um objeto JSON em um arquivo usando a função `dump()` e carregá-lo usando a função `load()`.

```Python
# Salvando
json_carro = json.dumps(carro)
with open('carro.json', 'w') as file:
  json.dump(carro, file)

# Carregando
with open('carro.json') as file:
  carro = json.load(file)
print(carro["marca"])
```

Isso produzirá a seguinte saída:

```
Tesla
```

## Aprofundando

Além das funções básicas para trabalhar com JSON, existem muitas outras ferramentas úteis e bibliotecas em Python que facilitam a leitura, escrita e manipulação de dados JSON, como `jsonschema` e `jsonpickle`. Além disso, é importante conhecer a estrutura básica de um objeto JSON e como ele pode ser representado em diferentes tipos de dados em Python, como listas e dicionários.

## Veja também

- [Documentação oficial do módulo JSON em Python](https://docs.python.org/3/library/json.html)
- [Tutorial de JSON em Python no Real Python](https://realpython.com/python-json/) 
- [Guia de referência rápida para trabalhar com JSON em Python](https://www.freecodecamp.org/news/python-parse-json/)