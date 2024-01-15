---
title:                "Encontrando o comprimento de uma string"
html_title:           "Javascript: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Ao trabalhar com strings em Javascript, é comum precisarmos saber o tamanho da string, ou seja, quantos caracteres ela possui. Saber como encontrar o comprimento de uma string pode ser útil em várias situações, como validar entrada de dados, manipular e exibir informações, entre outras.

## Como fazer

Para encontrar o comprimento de uma string em Javascript, podemos utilizar o método '.length'. Este método retorna o número total de caracteres da string. Vamos ver um exemplo:

```Javascript
let nome = "João";
console.log(nome.length); // Saída: 4
```

Aqui, declaramos uma variável 'nome' com o valor "João" e utilizamos o método '.length' para encontrar o seu comprimento, que é igual a 4. É importante lembrar que espaços e pontuações também são considerados na contagem.

Outra forma de encontrar o comprimento de uma string é utilizando o operador de propriedade '.length'. Veja o exemplo abaixo:

```Javascript
let frase = "Eu adoro programar!";
console.log(frase["length"]); // Saída: 21
```

Podemos notar que os dois métodos retornam o mesmo resultado. Agora, vamos ver como podemos utilizar o 'length' para validar a entrada de dados do usuário:

```Javascript
let email = prompt("Digite seu e-mail");

if (email.length > 0) {
  // continua o código
} else {
  alert("Por favor, insira um e-mail válido");
}
```

Ao utilizar o método '.length' para verificar se o usuário inseriu algum texto no campo de e-mail, podemos garantir que ele não deixe o campo em branco antes de prosseguir.

## Deep Dive

Em Javascript, cada caractere em uma string é indexado, ou seja, possui uma posição dentro da string. E o comprimento da string é igual ao número de caracteres que ela contém. Mas é importante lembrar que a contagem começa em 0, ou seja, o primeiro caractere tem o índice 0, o segundo o índice 1, e assim por diante.

Além disso, podemos encontrar o comprimento de uma string que contém caracteres especiais, como acentos e emojis, sem problemas. O método '.length' considera esses caracteres como um único caractere, independente de quantos bytes eles ocupam.

## Veja também

- [W3Schools - Javascript Strings](https://www.w3schools.com/js/js_strings.asp)
- [MDN Web Docs - String](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String)
- [DevMedia - Trabalhando com Strings em Javascript](https://www.devmedia.com.br/trabalhando-com-strings-em-javascript/38138)