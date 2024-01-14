---
title:    "Javascript: Extraindo Substrings"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair substrings é importante em programação?

Extrair substrings, ou seja, utilizar apenas parte de uma string, é uma habilidade essencial em programação. Isso pode ser útil para acessar informações específicas em uma grande string, para realizar operações em partes de um texto ou para formatar dados de entrada.

## Como extrair substrings com Javascript

Para extrair substrings em Javascript, podemos utilizar o método `substring()`. Ele recebe dois parâmetros: o índice inicial e o índice final da substring desejada. Por exemplo:

```Javascript
let string = "Este é um exemplo de string";
let substring = string.substring(0,4);
console.log(substring); //output: "Este"
```

É importante lembrar que o índice final não é incluído na substring, ou seja, no exemplo acima, a letra "u" (que está no índice 4) não é incluída.

## Uma análise mais profunda sobre a extração de substrings

Além do método `substring()`, também podemos utilizar o método `slice()`, que funciona de maneira semelhante. A diferença é que o `slice()` pode receber índices negativos, que contam a partir do fim da string. Por exemplo:

```Javascript
let string = "Dados de exemplo";
let substring = string.slice(-7); //retorna os últimos 7 caracteres
console.log(substring); //output: "exemplo"
```

Outra forma de extrair substrings é utilizando a notação de colchetes `[ ]`, que nos permite acessar uma posição específica de uma string. Por exemplo:

```Javascript
let string = "Este é o exemplo final";
let substring = string[7]; //pega o caractere no índice 7
console.log(substring); //output: "o"
```

## Veja também

- [Documentação do método substring() em Javascript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [Documentação do método slice() em Javascript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [Como utilizar a notação de colchetes em strings em Javascript](https://www.w3schools.com/jsref/jsref_string_brackets.asp)