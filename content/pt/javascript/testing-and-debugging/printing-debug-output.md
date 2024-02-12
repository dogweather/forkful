---
title:                "Exibindo saídas de depuração"
aliases: - /pt/javascript/printing-debug-output.md
date:                  2024-01-20T17:52:55.390003-07:00
model:                 gpt-4-1106-preview
simple_title:         "Exibindo saídas de depuração"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?

Debugar é tipo um GPS para desenvolvedores: ajuda a encontrar onde as coisas dão errado. Imprimir saídas de debug é deixar migalhas de pão pelo código, facilitando entender o que acontece durante a execução.

## Como Fazer:

Para mostrar mensagens de debug, usamos o `console.log()`. Simples, direto ao ponto.

```javascript
console.log('Olá, mundo!'); // Imprime: Olá, mundo!

let numero = 42;
console.log(numero); // Imprime: 42

function soma(a, b){
  console.log('A soma é:', a + b); // Mostra a soma no console
}

soma(10, 5); // Imprime: A soma é: 15
```

## Mergulho Profundo:

Historicamente, `console.log()` é o canivete suíço dos debugs. Existem outras ferramentas: `console.info()`, `console.warn()`, `console.error()`, variando no grau de severidade da mensagem. Em termos de implementação, o `console` é um objeto global em JavaScript com métodos para saída de dados.

Há também a opção de utilizar breakpoints e ferramentas de debug integradas ao navegador ou ao ambiente de desenvolvimento Node.js, mas isso é pós-graduação e foge ao `console.log()` 101.

## Veja também:

Para expandir seu repertório:

- [MDN Web Docs - Console](https://developer.mozilla.org/pt-BR/docs/Web/API/Console)
- [JavaScript Debugging](https://www.w3schools.com/js/js_debugging.asp)
- [Node.js debugging in VS Code](https://code.visualstudio.com/docs/nodejs/nodejs-debugging)

Agora é contigo. Parte para o código e deixa o `console.log()` ser teu pão de forma nas horas de aperto.
