---
date: 2024-01-19
description: "Como fazer: Para lidar com JSON no JavaScript, voc\xEA geralmente vai\
  \ ler um string JSON de alguma fonte e convert\xEA-lo num objeto JavaScript para\
  \ usar no seu\u2026"
lastmod: '2024-03-13T22:44:46.983523-06:00'
model: unknown
summary: "Para lidar com JSON no JavaScript, voc\xEA geralmente vai ler um string\
  \ JSON de alguma fonte e convert\xEA-lo num objeto JavaScript para usar no seu c\xF3\
  digo, ou ent\xE3o pegar um objeto JavaScript e convert\xEA-lo numa string JSON para\
  \ enviar para um servidor."
title: Trabalhando com JSON
weight: 38
---

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
