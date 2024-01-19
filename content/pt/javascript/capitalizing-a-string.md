---
title:                "Capitalizando uma string"
html_title:           "Javascript: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O Quê e Por Quê?

Capitalizar uma string significa transformar a primeira letra de cada palavra em maiúscula. Os programadores fazem isso para melhorar a legibilidade dos textos e dados do usuário.

## Como Faz:

Para capitalizar uma string no JavaScript, usamos a função `replace()`. Vamos dar uma olhada agora:

```Javascript
function capitalizarString(str) { 
  return str.replace(/\b\w/g, function(char) { 
    return char.toUpperCase(); 
  }); 
}

console.log(capitalizarString("olá, mundo!")); 
// Saída: "Olá, Mundo!"
```
Neste exemplo, `/\b\w/g` é uma expressão regular que encontra a primeira letra de cada palavra. A função então transforma cada letra encontrada em maiúscula.

## O Mergulho

A função `replace()` foi introduzida no JavaScript desde o seu início - a sua versatilidade e uso extensivo definem a sua longevidade. As alternativas para capitalização no JavaScript incluem o uso de `charAt()` com `substring()`, e a aplicação da função `map()` em um array criado através da função `split()`. 

No entanto, devemos ter cuidado com esses métodos em situações de alto desempenho. O método `replace()` com expressão regular pode ser mais lento que os outros quando lidando com textos muito grandes, pelo fato de que as expressões regulares tendem a ser mais lentas que os métodos de string nativa.

Aqui está um exemplo alternativo usando `charAt()` e `substring()`:
```Javascript
function capitalizarString(str) { 
  return str
    .split(' ')
    .map(word => word.charAt(0).toUpperCase() + word.substring(1))
    .join(' '); 
}

console.log(capitalizarString("olá, mundo!")); 
// Saída: "Olá, Mundo!"
```

## Veja Também

Para mais explicações e exemplos sobre modificações de string no JavaScript, você pode visitar os seguintes links:

1. [MDN Web Docs on replace()](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/replace) - Documentação oficial sobre a função `replace()`.
2. [MDN Web Docs on charAt()](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/charAt) - Documentação oficial sobre a função `charAt()`.
3. [MDN Web Docs on substring()](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/substring) - Documentação oficial sobre a função `substring()`.
4. [JavaScript.info: String Methods](https://javascript.info/string#capitalizing) - Uma visão geral útil de outros métodos de string no JavaScript.