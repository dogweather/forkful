---
title:                "Trabalhando com json"
html_title:           "Javascript: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/working-with-json.md"
---

{{< edit_this_page >}}

Trabalhando com JSON: Entenda e Aprenda

## O que é e por que programadores trabalham com JSON?

Trabalhar com JSON (JavaScript Object Notation) se trata de manipular dados estruturados no formato de texto, permitindo que informações sejam trocadas facilmente entre aplicações. Programadores utilizam JSON pois é uma maneira eficiente de transmitir e armazenar dados de forma organizada, além de ser uma linguagem de texto simples de ser compreendida. Ele é amplamente utilizado na web para enviar e receber dados entre servidores e clientes.

## Como fazer:

Trabalhar com JSON é simples e intuitivo. Veja alguns exemplos práticos de como utilizar JSON em código JavaScript:

Exemplo 1: Criando um objeto JSON com informações básicas:

``` Javascript
var carro = {
  marca: "Ford",
  modelo: "Mustang",
  ano: 1969
};

console.log(carro); // Imprime: { marca: "Ford", modelo: "Mustang", ano: 1969 }
```

Exemplo 2: Acessando informações específicas de um objeto JSON:

``` Javascript
var pessoa = {
  nome: "João",
  idade: 30,
  profissao: "Programador"
};

console.log(pessoa.nome); // Imprime: João
console.log(pessoa.profissao); // Imprime: Programador
```

Exemplo 3: Convertendo uma string JSON em um objeto JavaScript:

``` Javascript
var json = '{"nome":"Maria", "idade":25, "hobbies":["viajar", "ler"]}';
var pessoa = JSON.parse(json);

console.log(pessoa.hobbies[0]); // Imprime: viajar
```

## Aprofundando:

JSON foi criado originalmente por Douglas Crockford em 2001, como uma alternativa ao formato XML para armazenamento de dados. Apesar de ser muito utilizado na web, existem outras alternativas como YAML e BSON. A implementação de JSON em JavaScript é simples, já que a linguagem possui métodos específicos para manipular e converter dados neste formato.

## Veja também:

- [JSON.org](https://www.json.org/): Website oficial do formato JSON.
- [Documento da Mozilla sobre JSON](https://developer.mozilla.org/pt-BR/docs/Learn/JavaScript/Objects/JSON): Guia completo sobre como trabalhar com JSON em JavaScript.