---
title:                "Imprimindo saída de depuração"
html_title:           "Javascript: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## O que e por quê?

Imprimir saída de depuração é uma técnica comum usada por programadores para rastrear e solucionar problemas em seus códigos. Ao imprimir informações relevantes durante a execução de um programa, os programadores podem identificar onde e por que um erro ocorre.

## Como fazer:

Para imprimir saída de depuração em código Javascript, usamos a função `console.log()`. Esta função aceita qualquer tipo de dado como argumento e imprime-o no console do navegador. Veja o exemplo abaixo:

```Javascript
let num = 10;
let str = "Hello";
let arr = [1, 2, 3];
console.log(num); // saída: 10
console.log(str); // saída: Hello
console.log(arr); // saída: [1, 2, 3]
```

## Mergulho Profundo:

Imprimir saída de depuração existe há muito tempo e foi uma técnica amplamente utilizada por programadores antes da popularização das ferramentas de depuração. Hoje em dia, existem diferentes alternativas para imprimir saída de depuração, como usar um depurador integrado no navegador ou em um IDE. Além disso, a função `console.log()` tem uma versatilidade que permite mais do que simplesmente imprimir valores, ela também pode atribuir uma etiqueta ou um estilo específico para a saída. É importante remover todas as chamadas de `console.log()` antes de implementar o código em produção para evitar imprimir informações sensíveis ou desnecessárias.

## Veja Também:

- [Documentação do Console no MDN](https://developer.mozilla.org/pt-BR/docs/Web/API/Console)
- [Artigo sobre depuração em Javascript](https://www.digitalocean.com/community/tutorials/how-to-debug-javascript)
- [Vídeo sobre como imprimir saída de depuração no navegador](https://www.youtube.com/watch?v=3ntPiEG0z2w)