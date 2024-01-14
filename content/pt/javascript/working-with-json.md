---
title:                "Javascript: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## Por que trabalhar com JSON?

JSON (JavaScript Object Notation) é um formato de dados leve e amplamente utilizado para armazenar e transmitir informações. Ao trabalhar com JSON em programação, os desenvolvedores têm uma maneira rápida e eficiente de trocar dados entre diferentes sistemas e linguagens de programação. Com a popularidade de APIs (Application Programming Interface), que utilizam JSON para enviar e receber dados, é essencial para os programadores entenderem como manipular e trabalhar com esse formato de dados.

## Como fazer?

Primeiramente, é importante entender a estrutura básica de um objeto JSON. Um objeto é definido com chaves `{}` e contém pares de chave-valor separados por dois pontos `:`, como no exemplo abaixo:

```
let pessoa = { "nome": "Maria", "idade": 28, "profissão": "programadora" };
```

Para acessar os valores de um objeto JSON, podemos utilizar a notação de ponto `.`, como mostrado no exemplo abaixo:

```
console.log(pessoa.nome); // output: Maria
```

Para adicionar novos valores a um objeto JSON, basta utilizar a notação de ponto e atribuir um novo valor à chave desejada. Além disso, também é possível converter um objeto JSON em uma string utilizando o método `JSON.stringify()`, ou converter uma string em um objeto JSON utilizando o método `JSON.parse()`.

Além de objetos, JSON também pode ser utilizado para armazenar e transmitir arrays. Um array é definido com colchetes `[]` e pode conter vários valores separados por vírgula, como no exemplo abaixo:

```
let frutas = ["maçã", "banana", "morango"];
```

Para acessar um valor específico em um array JSON, podemos utilizar o índice do elemento desejado, assim como em arrays comuns. Veja o exemplo abaixo:

```
console.log(frutas[0]); //output: maçã
```

## Mergulho profundo

Uma das características mais úteis do JSON é que ele é muito fácil de ser lido e manipulado por seres humanos. A indentação correta e o uso correto de aspas são fundamentais para garantir que um objeto JSON seja válido. Além disso, o uso de métodos como `JSON.stringify()` e `JSON.parse()` pode tornar o trabalho com JSON ainda mais fácil.

Quando se trata de APIs, é importante ter em mente que nem sempre as chaves de um objeto JSON serão as mesmas para todas as requisições. Por isso, é necessário verificar sempre a documentação da API e entender a estrutura dos dados que serão retornados.

## Veja também
- [Documentação do JSON na MDN (em inglês)](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/JSON)
- [Trabalhando com JSON em Node.js (em português)](https://blog.geekhunter.com.br/trabalhando-com-json-em-node-js/)
- [Criando e manipulando JSON com jQuery (em português)](https://www.devmedia.com.br/criando-e-manipulando-json-com-jquery/28150)