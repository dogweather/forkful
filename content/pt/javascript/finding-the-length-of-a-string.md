---
title:                "Javascript: Encontrando o comprimento de uma string"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Você provavelmente já se deparou com situações em que precisava descobrir a quantidade de caracteres em uma determinada palavra ou frase. Encontrar o comprimento de uma string é uma tarefa comum na programação, útil para validar inputs de usuário, construir aplicativos de formatação de texto e muito mais.

## Como fazer

Para encontrar o comprimento de uma string, é necessário usar a propriedade `length` do objeto string. Confira o exemplo abaixo:

```Javascript
let nome = "Joana";
console.log(nome.length);

// Output: 5 (a string "Joana" tem cinco caracteres)
```

Você também pode usar essa propriedade diretamente em uma string sem atribuí-la a uma variável, como mostrado abaixo:

```Javascript
console.log("Hello World!".length);

// Output: 12 (a frase "Hello World!" tem doze caracteres)
```

## Mergulho Profundo

Agora que aprendemos a usar a propriedade `length` para encontrar o comprimento de uma string, é importante entender que ela conta todos os caracteres, incluindo espaços em branco e pontuações. Além disso, essa propriedade não funciona em strings vazias ou em outros tipos de dados, como números ou booleanos.

Por fim, é importante saber que essa propriedade é apenas para leitura e não pode ser alterada ou atribuída a um novo valor.

## Veja também

- [MDN Web Docs: String length property](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [W3Schools: Javascript String length Property](https://www.w3schools.com/jsref/jsref_length_string.asp)
- [freeCodeCamp: Get the Length of a String](https://www.freecodecamp.org/forum/t/get-the-length-of-a-string/219256)