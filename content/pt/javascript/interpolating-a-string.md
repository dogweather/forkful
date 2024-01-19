---
title:                "Interpolando uma string"
html_title:           "Java: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Interpolação de String em JavaScript: O Que, Porquê e Como

## O Que e Porquê?

Interpolação de string em JavaScript envolve inserir expressões ou variáveis ​​dentro de strings para tornar o código mais flexível e dinâmico. Isso proporciona uma concatenação de string mais limpa e eficiente, tornando o código mais legível.

## Como fazer:

Há uma maneira simples de fazer isso em JavaScript moderno, usando as chamadas 'template strings'. Aqui está um exemplo:

```Javascript
let nome = "Maria";
let saudacao = `Olá, ${nome}!`;
console.log(saudacao); // irá imprimir 'Olá, Maria!'
```

As variáveis podem ser inseridas diretamente nas strings, útil especialmente ao gerar dinamicamente respostas de texto.

## Mergulho Profundo

Historicamente, a concatenação de string em JavaScript era feita com o operador '+', que podia ser confuso e propenso a erros. A interpolação de string foi introduzida no ES6 para resolver esses problemas. 

Existem alternativas em bibliotecas externas, como lodash e outras, mas a maneira mais moderna e direta é usando o recurso embutido do JavaScript.

A interpolação de string no JavaScript usa a função 'toString' no pano de fundo para garantir que todas as variáveis sejam convertidas corretamente em uma string.

## Veja também

- [MDN - Template Strings](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/template_strings)
- [JavaScript.info - String Interpolation](https://javascript.info/string#string-interpolation)

Ao longo do artigo você perceberá o quão útil, poderoso e limpo é o uso de interpolação de strings em JavaScript.