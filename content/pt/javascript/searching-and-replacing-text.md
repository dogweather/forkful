---
title:                "Procurando e substituindo texto"
html_title:           "Javascript: Procurando e substituindo texto"
simple_title:         "Procurando e substituindo texto"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que
Você já se deparou com a necessidade de substituir um determinado texto em um arquivo ou em uma grande quantidade de dados? Sabia que o Javascript possui uma função específica para isso? Aprenda a usar o método "replace()" e facilite suas tarefas de busca e substituição de texto!

## Como fazer
Para substituir um texto em Javascript, utilizamos o método "replace()". A sintaxe é simples: primeiro, informamos o texto que queremos substituir entre aspas, seguido pelo texto que será inserido no lugar. Veja um exemplo:

```Javascript
let texto = "Olá, mundo!";
let novoTexto = texto.replace("mundo", "meu amigo");
console.log(novoTexto); // Saída: Olá, meu amigo!
```

Podemos também usar o método "replace()" com expressões regulares para buscar e substituir padrões específicos. Por exemplo, para substituir todas as ocorrências de um determinado nome em um texto, podemos utilizar o símbolo "g" (global) junto com a expressão regular, como mostrado no exemplo abaixo:

```Javascript
let texto = "Olá, João! Como vai, João?";
let novoTexto = texto.replace(/João/g, "Pedro");
console.log(novoTexto); // Saída: Olá, Pedro! Como vai, Pedro?
```

## Mergulho profundo
Além da função básica de buscar e substituir textos, o método "replace()" também permite utilizar uma função como segundo parâmetro. Essa função é executada para cada correspondência encontrada na string, permitindo um maior controle sobre a substituição.

Além disso, ao utilizar uma expressão regular como primeiro parâmetro, é possível utilizar "grupos de captura" para inserir partes específicas do texto substituto. Veja um exemplo:

```Javascript
let texto = "Meu nome é Marcos, e eu gosto de programar!";
let novoTexto = texto.replace(/(Marcos),/g, "Meu nome é $1,");
console.log(novoTexto); // Saída: Meu nome é Marcos, e eu gosto de programar!
```

Neste exemplo, utilizamos os parênteses para criar um "grupo de captura" com o nome "Marcos". Em seguida, no texto substituto, podemos inserir esse grupo utilizando o símbolo "$" seguido pelo número do grupo. Isso permite substituir apenas uma parte do texto enquanto mantém o restante intacto.

## Veja também
- [Documentação do método "replace()"](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Tutorial sobre substituição de textos com expressões regulares](https://www.devmedia.com.br/usando-expressoes-regulares-em-javascript/39019)
- [Videoaula explicando como substituir textos em Javascript](https://www.youtube.com/watch?v=cpE58GvuGHM)