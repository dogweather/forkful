---
title:                "Lidando com json."
html_title:           "TypeScript: Lidando com json."
simple_title:         "Lidando com json."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/working-with-json.md"
---

{{< edit_this_page >}}

##Por que

O JavaScript Object Notation (JSON) é uma formato de dados leve, que oferece uma maneira rápida e fácil de trocar informações entre diferentes sistemas. Ao trabalhar com JSON, é possível armazenar e transmitir dados de forma mais eficiente, otimizando o desempenho do seu código.

## Como Fazer

Para trabalhar com JSON em TypeScript, é necessário importar o módulo 'JSON' e utilizar seus métodos para realizar operações como parse e stringifiy (transformar um objeto em uma string e vice-versa). Veja um exemplo de como isso pode ser feito:

```TypeScript
import * as JSON from 'JSON';

//Objeto que será transformado em uma string JSON
const person = {
  nome: "João",
  idade: 30,
  cidade: "São Paulo"
}

//Transformando o objeto em uma string JSON
const personString = JSON.stringify(person);
console.log(personString); //{"nome":"João","idade":30,"cidade":"São Paulo"}

//Convertendo a string JSON de volta para um objeto
const personObject = JSON.parse(personString);
console.log(personObject); //{nome: "João", idade: 30, cidade: "São Paulo"}
```

## Profundidade

Uma das maiores vantagens de se trabalhar com JSON é sua compatibilidade com diferentes linguagens de programação. Isso permite que um sistema desenvolvido em TypeScript, por exemplo, possa se comunicar facilmente com um sistema em Java ou Python, compartilhando dados no formato JSON.

Além disso, o JSON também é compatível com a maioria dos bancos de dados, facilitando o armazenamento e recuperação de informações no formato JSON.

Outra característica interessante é a possibilidade de se trabalhar com JSON direto no navegador, sem a necessidade de um servidor. Isso é possível graças à presença do objeto "JSON" na especificação do ECMAScript desde o ES5.

## Veja Também

- [Documentação oficial do JSON](https://www.json.org/json-pt.html)
- [Guia de syntax para JSON em TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-9.html#json)
- [Carregando arquivos JSON em um projeto TypeScript](https://medium.com/@kevinkreuzer/storing-and-loading-simple-data-on-a-typescript-project-9bdbfbeed3ce)