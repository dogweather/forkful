---
title:    "Javascript: Capitalizando uma string"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Por que capitalizar uma string?

Capitalizar uma string é uma tarefa comum em programação e pode ser útil em várias situações, como por exemplo, formatar dados de entrada para que fiquem legíveis ou para melhorar a estética de um texto em uma página da web.

## Como fazer?

Existem várias maneiras de capitalizar uma string em Javascript, mas a mais simples é utilizando o método `toUpperCase()`. Veja um exemplo abaixo:

```Javascript
let string = "exemplo de texto";
let stringCapitalizada = string.toUpperCase();

console.log(stringCapitalizada); // saída: EXEMPLO DE TEXTO
```

Além disso, é possível utilizar regex para capitalizar a primeira letra de cada palavra em uma string, como no exemplo abaixo:

```Javascript
let string = "exemplo de texto";
let stringCapitalizada = string.replace(/\w\S*/g, function(texto) {
    return texto.charAt(0).toUpperCase() + texto.substr(1).toLowerCase();
});

console.log(stringCapitalizada); // saída: Exemplo De Texto
```

## Aprofundando-se

Além dos métodos mencionados acima, existem várias outras formas de capitalizar uma string em Javascript. Uma delas é utilizando a biblioteca lodash que oferece a função `capitalize()` que permite capitalizar uma string de forma mais flexível, podendo especificar qual caractere deve ser utilizado como delimitador de palavras.

Outra opção é utilizar o método `replace()` em conjunto com uma expressão regular para substituir a primeira letra de cada palavra pela sua versão capitalizada.

Independentemente do método escolhido, é importante lembrar que strings em Javascript são imutáveis, ou seja, não podem ser alteradas diretamente. Portanto, todas as funções de capitalização irão retornar uma nova string, sem modificar a original.

## Veja também

- [MDN Web Docs: toUpperCase()](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [MDN Web Docs: replace()](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Lodash Docs: capitalize()](https://lodash.com/docs/4.17.15#capitalize)