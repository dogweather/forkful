---
title:                "Javascript: Excluindo caracteres que coincidem com um padrão"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

Por que Excluir Caracteres Correspondentes a um Padrão?

Em algumas situações de programação, é necessário realizar a exclusão de caracteres que correspondam a um determinado padrão. Isso pode ser útil em tarefas de formatação de texto, validação de dados ou até mesmo para melhorar a performance de um algoritmo. Neste artigo, aprenderemos como fazer isso em Javascript.

Como Fazer

Para excluir caracteres que correspondam a um padrão em Javascript, precisamos utilizar expressões regulares. Isso nos permite buscar por determinados padrões de caracteres em uma string e realizar alterações conforme necessário.

Vamos dar um exemplo prático. Digamos que temos uma string contendo uma série de números e desejamos excluir todos os caracteres que não sejam numéricos. Podemos fazer isso utilizando a seguinte expressão regular:

```Javascript
let string = "1a2b3c4d5e6f";
string = string.replace(/[^0-9]/g, "");
console.log(string); // saída: 123456
```

Neste exemplo, utilizamos o método `replace` para substituir todos os caracteres que não sejam números por uma string vazia `""`. O modificador `g` indica que a busca deve ser global, ou seja, todos os caracteres que correspondam ao padrão devem ser alterados.

Além disso, também podemos utilizar as expressões regulares em conjunto com a função `filter` para excluir caracteres de um array que não correspondam ao padrão desejado. Veja o exemplo abaixo:

```Javascript
let array = ["abc123", "456def", "789ghi"];
array = array.filter(item => item.match(/^[0-9]+$/));
console.log(array); // saída: ["456", "789"]
```

Neste caso, utilizamos a função `match` para verificar se a string contém apenas números. Se sim, ela é adicionada ao novo array. Caso contrário, é ignorada.

Profundidade

As expressões regulares em Javascript são extremamente versáteis e podem ser utilizadas em diversas situações, não apenas para excluir caracteres correspondentes a um padrão. Podemos utilizar também para buscar, substituir, validar, entre outras funções.

Além disso, as expressões regulares podem conter diversas opções de modificadores e metacaracteres, o que permite criar padrões cada vez mais específicos e avançados.

Portanto, é importante dedicar um tempo para aprender e se familiarizar com o uso das expressões regulares em Javascript, pois elas podem ser muito úteis no dia a dia de um programador.

Veja Também

- [Expressões Regulares em Javascript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Regular_Expressions)
- [String.prototype.replace()](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Array.prototype.filter()](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/Array/filter)