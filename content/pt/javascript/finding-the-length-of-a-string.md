---
title:                "Javascript: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Se você é novo na programação ou está aprendendo sobre Javascript, provavelmente já se deparou com o desafio de encontrar o comprimento de uma string. Enquanto isso pode parecer trivial para alguns, a habilidade de calcular o tamanho de uma string é fundamental para muitas tarefas de programação.

## Como fazer

Para encontrar o comprimento de uma string em Javascript, podemos usar o método `length`. Por exemplo, se queremos encontrar o comprimento da string "Olá mundo!", podemos escrever o seguinte código:

```Javascript
let minhaString = "Olá mundo!";
console.log(minhaString.length); //output: 11
```

Podemos ver que a propriedade `length` retorna o número de caracteres na string, incluindo espaços em branco e pontuação.

Outra maneira de encontrar o comprimento de uma string é usar a função `strlen()` do Javascript, que exige que coloquemos a string como argumento entre parênteses. Por exemplo:

```Javascript
let minhaString = "Bem-vindo!";
console.log(strlen(minhaString)); //output: 10
```

## Mergulhando mais fundo

Quando se trata de encontrar o comprimento de uma string, é importante entender como o Javascript armazena e manipula as strings. Em vez de um único tipo de dado, as strings no Javascript são objetos, o que significa que elas possuem propriedades e métodos que podemos acessar. O método `length` é uma dessas propriedades.

Também é importante observar que o `length` não é um índice ou posição, mas sim uma propriedade que retorna o tamanho da string. Por exemplo, se tentarmos acessar `minhaString[10]`, receberemos `undefined`, porque a última posição válida para esta string é 9.

Por fim, vale ressaltar que o método `length` é sensível ao case, o que significa que ele contará as letras maiúsculas e minúsculas como caracteres diferentes. Portanto, é importante ter isso em mente ao calcular o comprimento de uma string.

## Veja também

- <https://www.w3schools.com/js/js_string_length.asp>
- <https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/length>
- <https://www.digitalocean.com/community/tutorials/construindo-uma-funcao-para-encontrar-o-comprimento-das-strings-em-javascript>