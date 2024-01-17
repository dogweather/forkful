---
title:                "Trabalhando com json"
html_title:           "Gleam: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Você provavelmente já se deparou com o termo JSON em um projeto de programação, mas o que ele significa? JSON é uma sigla para "JavaScript Object Notation" (Notação de Objetos JavaScript) e é uma maneira comum de representar dados em formato de texto. Como linguagens de programação lidam com dados de maneira diferente, é necessário que eles tenham um formato universal como JSON para compartilhá-los e processá-los facilmente.

## Como fazer:
Agora, vamos dar uma olhada no básico de como trabalhar com JSON em Gleam. Primeiro, precisamos importar o módulo padrão ```json``` com o comando ```import json```. Em seguida, podemos usar a função ```json.from_string``` para analisar uma string de JSON em um objeto Gleam. Por exemplo:
```
import json

let json_str = "{\"nome\": \"João\", \"idade\": 25}"
let json_obj = json.from_string(json_str)
```
Para acessar valores específicos em nosso objeto JSON, podemos usar a função ```json.get``` seguida pelo caminho até o valor desejado, usando uma lista de strings. Por exemplo, se quisermos acessar o valor da chave "idade", poderíamos fazer:
```
let idade_val = json.get(json_obj, ["idade"])
```
O valor retornado seria um tipo de dado chamado ```Result```, que contém o valor da idade se tudo correr bem ou um erro se não for possível encontrá-la.

## Profundando:
JSON foi criado originalmente por Douglas Crockford em 2001 e se tornou uma maneira popular de transmitir dados em uma API da web. Existem outras alternativas, como XML, mas JSON é mais simples e fácil de entender para humanos e máquinas. Em Gleam, o módulo padrão ```json``` é baseado em uma biblioteca C chamada Jansson, que é usada para converter entre JSON e uma "árvore" de valores em C.

## Veja também:
Para saber mais sobre como trabalhar com JSON em Gleam, confira a documentação oficial: https://gleam.run/modules/json.html.
Para entender mais sobre a história e o uso de JSON, confira o site da IETF (Internet Engineering Task Force): https://tools.ietf.org/html/rfc8259.