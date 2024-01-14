---
title:    "Javascript: Encontrando o comprimento de uma string"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que encontrar o comprimento de uma string?
Ao desenvolver um código, é comum ter a necessidade de manipular strings. Saber o comprimento de uma string é fundamental para diversas tarefas, como validação de entrada de dados, formatação de texto e manipulação de arrays. Neste artigo, vamos aprender como encontrar o comprimento de uma string em Javascript e explorar um pouco mais sobre esse conceito.

## Como fazer isso:
Para encontrar o comprimento de uma string em Javascript, podemos utilizar o método `length`. Ele retorna o número de caracteres presentes na string, incluindo espaços e pontuações.

```Javascript
let string = "Olá! Eu sou uma string.";
console.log(string.length);
```

A saída desse código será `22`, pois a string contém 22 caracteres.

Podemos também utilizar o método `length` para verificar se uma string está vazia, pois se o seu comprimento for igual a 0, significa que não há nenhum caractere presente.

```Javascript
let string = "";
if (string.length === 0) {
  console.log("A string está vazia!");
};
```

## Deep Dive:
É importante ressaltar que o método `length` não conta apenas letras, mas sim qualquer tipo de caractere, incluindo acentos e símbolos especiais. Também é necessário ter cuidado com a contagem de espaços, pois eles também são incluídos no comprimento da string.

É interessante notar que strings vazias ou nulas não possuem o método `length`, portanto uma verificação é necessária quando se trabalha com esses tipos de dados.

Outro ponto importante a ser observado é que o método `length` é apenas uma propriedade das strings em Javascript e não uma função, por isso não é necessário utilizar parênteses ao chamá-lo.

## Veja também:
- [Documentação do método `length` em strings](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Manipulando strings em Javascript](https://www.devmedia.com.br/manipulando-strings-em-javascript/38488)
- [Validando input de dados com Javascript](https://www.treinaweb.com.br/blog/validando-input-de-dados-com-o-html5-e-javascript/)