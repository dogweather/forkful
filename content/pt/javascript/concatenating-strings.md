---
title:    "Javascript: Concatenando strings"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que

Concatenar strings é uma habilidade fundamental para qualquer programador JavaScript. Ao unir duas ou mais strings, podemos criar mensagens personalizadas, escrever informações em um formato específico ou até mesmo construir URLs dinâmicas. Em resumo, a concatenação de strings nos permite manipular texto de forma eficiente e eficaz.

## Como fazer

Para concatenar strings em JavaScript, podemos usar o operador `+` ou o método `concat()`. Veja abaixo dois exemplos de código:

```Javascript
// Usando o operador '+'
let nome = "Maria";
let sobrenome = "Silva";
let nomeCompleto = nome + " " + sobrenome;
console.log(nomeCompleto); // Saída: Maria Silva

// Usando o método 'concat()'
let fruta = "maçã";
let texto = "Eu gosto de comer ";
let frase = texto.concat(fruta);
console.log(frase); // Saída: Eu gosto de comer maçã
```

Perceba que ambos os métodos produzem o mesmo resultado, mas o uso de `concat()` pode ser mais útil quando precisamos unir mais de duas strings. Além disso, o operador `+` também pode ser usado para concatenar números e strings, mas é importante lembrar que o resultado será uma nova string.

## Mergulho profundo

É importante mencionar que o método `concat()` não altera os valores das strings originais, mas cria uma nova string contendo a concatenação. Além disso, podemos utilizar templates de string em ECMAScript 6 para realizar concatenações de forma mais elegante e legível.

Outro aspecto interessante da concatenação de strings é o uso de placeholders, como `%s` e `%d`, que servem para inserir valores de variáveis em uma string formatada. Veja um exemplo:

```Javascript
let fruta = "maçã";
let preço = 2.50;
let frase = `Uma ${fruta} custa R$%d reais.`;
console.log(frase, preço); // Saída: Uma maçã custa R$2.50 reais.
```

## Veja também

- Documentação do método `concat()`: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/concat
- Manipulando strings com ECMAScript 6: https://www.freecodecamp.org/news/manipulating-strings-in-javascript-es6-part-1-2005e796d9fe/
- Utilizando placeholders em strings formatadas: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Template_literals