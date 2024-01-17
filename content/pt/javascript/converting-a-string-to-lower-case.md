---
title:                "Convertendo uma string para minúsculas"
html_title:           "Javascript: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O que e Por Que?

Conversão de string para caixa baixa é um procedimento comum realizado por programadores em JavaScript para transformar todas as letras maiúsculas em minúsculas. Isso pode ser útil em cenários como validação de entrada de dados do usuário, comparação de strings e formatação de texto.

## Como Fazer:

```Javascript
let myString = "HELLO WORLD";
console.log(myString.toLowerCase()); // Output: "hello world"
```

Em JavaScript, podemos usar o método `toLowerCase()` para converter uma string para caixa baixa. Ele pode ser chamado em qualquer variável de string e retorna uma nova string com todos os caracteres em caixa baixa.

Podemos aplicar esse método em conjunção com outros métodos, como `trim()` para remover quaisquer espaços em branco e `replace()` para substituir caracteres específicos antes da conversão para caixa baixa.

## Deep Dive:

A conversão de string para caixa baixa tem sido uma parte fundamental da programação desde os primeiros dias da linguagem. Antes do JavaScript, o processamento de texto era feito principalmente em linguagens de baixo nível, o que tornava o processo mais complexo e demorado.

Uma alternativa ao método `toLowerCase()` é o uso de expressões regulares para encontrar e substituir caracteres específicos em strings. No entanto, isso pode ser mais trabalhoso e menos eficiente em comparação com o método nativo do JavaScript.

Ao converter uma string para caixa baixa, é importante estar ciente da diferença entre caracteres maiúsculos e minúsculos em diferentes idiomas. Por exemplo, em alguns idiomas, a letra "i" pode ser escrita de diferentes maneiras em maiúsculas e minúsculas, o que pode afetar a validade de uma comparação de strings.

## Veja Também:

- [Método toLowerCase() em MDN](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)