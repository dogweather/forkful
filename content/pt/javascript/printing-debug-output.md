---
title:                "Imprimindo saída de debug"
html_title:           "C#: Imprimindo saída de debug"
simple_title:         "Imprimindo saída de debug"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## O que é e por quê?

Imprimir saída de debug é o processo de exibir informações que podem ajudar a identificar e resolver problemas no código. Os programadores fazem isso para entender o que está acontecendo "por baixo dos panos" e corrigir erros de forma mais eficiente.

## Como fazer:

Vamos usar a função nativa console.log() do JavaScript para imprimir a saída de debug. Veja o exemplo abaixo. 

```Javascript
var x = 5;
var y = 10;
console.log('O valor de x é', x, 'e o valor de y é', y);
```

Isso imprimirá na console: "O valor de x é 5 e o valor de y é 10"

## Mergulho Profundo: 

A saída de debug não é nova, ela vem desde os primeiros dias da programação. O console.log() é amplamente utilizado no JavaScript, mas existem alternativas, como console.info(), console.error() e console.warn(). Eles funcionam de maneira semelhante, mas são usados em diferentes contextos.

A implementação da impressão de debug pode variar dependendo do ambiente de execução do JavaScript. No Node.js, console.log() imprime na saída padrão, enquanto no navegador, ele imprime no console do Desenvolvedor. 

## Veja Também:

Para mais informações, você pode consultar estes recursos:

- Documentação oficial do Console no MDN: [MDN Console](https://developer.mozilla.org/pt-BR/docs/Web/API/Console)
- Artigos sobre Debugging no JavaScript: [JavaScript Debugging](https://www.w3schools.com/js/js_debugging.asp).