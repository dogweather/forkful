---
title:    "Javascript: Encontrando o comprimento de uma string"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Por que
Encontrar o comprimento de uma string é uma tarefa comum na programação JavaScript. Saber como fazer isso pode ajudar a resolver problemas e manipular dados de maneira mais eficiente. Nesta postagem, vamos explorar algumas maneiras de encontrar o comprimento de uma string em JavaScript.

## Como fazer
Existem várias maneiras de encontrar o comprimento de uma string em JavaScript. Vamos dar uma olhada em algumas delas usando exemplos de código.

```Javascript
const string = "Olá, mundo!";
console.log(string.length);
// output: 12
```
Usando o método `length` embutido, podemos encontrar o comprimento de uma string facilmente. Ele retorna o número de caracteres em uma string, incluindo espaços.

```Javascript
const string = "Hoje é um dia ensolarado";
console.log(string.length);
// output: 26
```

Além do método `length`, também podemos usar o loop `for` para iterar sobre cada caractere da string e contar quantas vezes o loop foi executado. Vamos ver um exemplo:

```Javascript
const string = "Uma longa string";
let len = 0;
for (let i=0; i < string.length; i++) {
    len++;
}
console.log(len);
// output: 17
```

Nesse exemplo, a variável `len` começa com o valor 0 e é incrementada em 1 a cada iteração do loop `for`, até que todas as letras da string sejam percorridas.

## Deep Dive
É importante notar que o método `length` retorna a quantidade de caracteres em uma string, incluindo espaços em branco e caracteres especiais. Por exemplo, se uma string contém um emoji, ele será contado como um caractere, mesmo que possa parecer dois ou mais caracteres visuais.

Além disso, o método `length` retorna um número, mas esse número não representa a posição dos caracteres na string. Em outras palavras, ele não retorna a posição de um determinado caractere em uma string. Para isso, podemos usar o método `indexOf`.

## Veja também
- [Documentação sobre o método length](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Exemplo de uso do método indexOf](https://www.w3schools.com/jsref/jsref_indexof.asp)
- [Outros métodos úteis para manipular strings em JavaScript](https://www.digitalocean.com/community/tutorials/how-to-work-with-strings-in-javascript)