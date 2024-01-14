---
title:    "Javascript: Utilizando expressões regulares"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Por que utilizar Expressões Regulares em Javascript?

As Expressões Regulares, também conhecidas como regex, são uma poderosa ferramenta para manipular e pesquisar textos em Javascript. Ao utilizá-las, é possível realizar tarefas como validação de dados, substituição de texto e extração de informações específicas de uma string. Se você quer economizar tempo e tornar seu código mais eficiente, as regex são uma ótima opção.

## Como utilizar Expressões Regulares em Javascript

Para utilizar Expressões Regulares em Javascript, primeiro é necessário criar um objeto Regex utilizando a sintaxe ```javascript /padrão/a```, onde "padrão" é a expressão que você deseja procurar. Em seguida, é possível utilizar métodos como ```test()```, que retorna true se o padrão for encontrado na string, ou ```exec()```, que retorna informações sobre a ocorrência do padrão.

Por exemplo, se quisermos verificar se uma string possui apenas números, podemos utilizar o seguinte código:

```javascript
let regex = /[0-9]+/; // regex para encontrar números
let str = "123abc";
let result = regex.test(str); // retorna true
```

## Aprofundando em Expressões Regulares

Existem várias opções de modificadores e caracteres especiais que podem ser utilizados para tornar as Expressões Regulares mais precisas e flexíveis. Alguns deles são:

- **i**: torna a busca case-insensitive, ou seja, não diferencia maiúsculas de minúsculas. Ex: ```/[A-Z]+/i``` encontra tanto "ABC" quanto "AbC";

- **g**: faz com que a busca retorne todas as ocorrências do padrão na string, não apenas a primeira. Ex: ```/abc/g``` encontra todas as ocorrências de "abc" em uma string;

- **^**: indica que o padrão deve ser encontrado no início da string;

- **$**: indica que o padrão deve ser encontrado no final da string;

- **.**: representa qualquer caractere;

- **[]**: define um conjunto de caracteres possíveis para o padrão. Por exemplo, ```/[aeiou]/``` encontra todas as vogais em uma string.

Esses são apenas alguns exemplos, mas é possível utilizar muitos outros modificadores e caracteres especiais para criar expressões regulares ainda mais complexas e precisas.

## Veja também

Aqui estão alguns links úteis para aprofundar seus conhecimentos em Expressões Regulares em Javascript:

[MDN - Expressões Regulares](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Regular_Expressions)

[Tutorial - Expressões Regulares em Javascript](https://www.ramosdainformatica.com.br/criando-expressoes-regulares-em-javascript/)

[Livro - Expressões Regulares: Uma abordagem divertida](https://www.casadocodigo.com.br/products/livro-regex)

Agora que você já sabe como utilizar estas poderosas ferramentas em seu código Javascript, experimente e veja como elas podem facilitar sua vida como programador. Boa sorte!