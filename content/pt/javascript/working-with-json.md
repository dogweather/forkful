---
title:                "Trabalhando com JSON"
html_title:           "Arduino: Trabalhando com JSON"
simple_title:         "Trabalhando com JSON"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## O que é & Por quê?
JSON, ou JavaScript Object Notation, é um formato leve de troca de dados. Programadores utilizam JSON para transferir e armazenar dados de maneira simples e rápida entre o servidor e o cliente e porque é fácil de ler e escrever para humanos.

## Como fazer:
Para lidar com JSON no JavaScript, você geralmente vai ler um string JSON de alguma fonte e convertê-lo num objeto JavaScript para usar no seu código, ou então pegar um objeto JavaScript e convertê-lo numa string JSON para enviar para um servidor.

```Javascript
// String JSON
const jsonString = '{"nome": "João", "idade": 30, "cidade": "Lisboa"}';

// Convertendo string JSON para um objeto JavaScript
const usuario = JSON.parse(jsonString);
console.log(usuario.nome); // Saída: João

// Criando um objeto JavaScript
const carro = { marca: "Fiat", modelo: "500", cor: "branco" };

// Convertendo o objeto JavaScript para string JSON
const jsonCarro = JSON.stringify(carro);
console.log(jsonCarro); // Saída: {"marca":"Fiat","modelo":"500","cor":"branco"}
```

## Aprofundando:
JSON surgiu a partir da necessidade de um formato de comunicação de dados que fosse simples e rápido de ser interpretado, tanto por máquinas quanto por humanos, inspirado na notação literal de objetos do JavaScript. Alternativas incluem XML, que é mais verboso e lento para ser processado, e YAML, que é mais legível por humanos, mas menos amplamente adotado. Ao trabalhar com JSON, é crucial entender como evitar e lidar com erros de parse e de formatação, como o uso incorreto de aspas duplas, e garantir que a codificação de caracteres esteja correta (geralmente UTF-8).

## Veja também:
- Documentação oficial do JSON: https://www.json.org/json-pt.html
- Guia MDN sobre JSON: https://developer.mozilla.org/pt-BR/docs/Learn/JavaScript/Objects/JSON
- JSON Validator para checar a formatação: https://jsonlint.com/
