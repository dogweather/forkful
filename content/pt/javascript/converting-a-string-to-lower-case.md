---
title:    "Javascript: Convertendo uma string para minúsculas"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que converter uma string para letra minúscula?

A conversão de uma string para letra minúscula é útil em várias situações de programação, principalmente quando se trabalha com entrada de dados do usuário. Isso permite que as strings se tornem mais uniformes e facilita a comparação e processamento de dados.

## Como fazer

Para converter uma string para letra minúscula em Javascript, pode-se utilizar o método `toLowerCase()`. Este método retorna uma nova string com todas as letras da string original em minúsculo.

```javascript
let string = "EXEMPLO DE STRING";
let lowerCase = string.toLowerCase();

console.log(lowerCase); // output: exemplo de string
```

Outra forma de converter uma string para letra minúscula é utilizando o operador de atribuição `+=` junto com o método `toLowerCase()`.

```javascript
let string = "EXEMPLO DE STRING";
string += string.toLowerCase();

console.log(string); // output: EXEMPLO DE STRINGexemplo de string
```

## Deep Dive

Por baixo dos panos, a conversão de uma string para letra minúscula é feita usando código ASCII (American Standard Code for Information Interchange). Cada caractere tem um número associado de acordo com a tabela ASCII e o método `toLowerCase()` usa esses números para converter as letras maiúsculas em minúsculas.

Existem também casos especiais, como as letras acentuadas em português. Nesse caso, o código ASCII não é utilizado e o método `toLowerCase()` tem conhecimento da língua e suas regras de conversão.

## Veja também

- Documentação do método `toLowerCase()` no MDN: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase
- Tabela ASCII: https://www.ascii-code.com/