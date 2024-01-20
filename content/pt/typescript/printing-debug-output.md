---
title:                "Imprimindo saída de debug"
html_title:           "C#: Imprimindo saída de debug"
simple_title:         "Imprimindo saída de debug"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

# Debugando com TypeScript: A arte de Imprimir Saídas de Depuração

## O que & Por quê?

Imprimir saídas de depuração refere-se à prática de usar comandos de impressão (como `console.log`, `console.error` e `console.warn`) para rastrear o fluxo de execução do código. A importância disso? Permite que os programadores identifiquem e resolvam problemas ou verifiquem o valor de variáveis em tempo de execução.

## Como Fazer:

É fácil imprimir saídas de depuração no TypeScript. Basta usar o objeto console incorporado. Ele fornece métodos como `log()`, `error()` e `warn()`. Aqui está como:

```TypeScript
let foo = 1;
console.log('O valor de foo é', foo); // Saída: O valor de foo é 1

try {
  throw new Error('Um erro ocorreu!');
} catch (e) {
  console.error(e.message); // Saída: Um erro ocorreu!
}
```

## Mergulhando Mais Fundo:

Historicamente, a impressão de depuração deriva do conceito de "printf debugging" em C, que envolve a inserção de declarações printf para inspecionar os valores durante a execução. 

Alternativamente, o uso de depuradores dedicados, como o Chrome DevTools para JavaScript, oferece uma experiência de depuração mais avançada. Eles permitem a definição de pontos de interrupção, a execução passo a passo e o exame detalhado do estado do aplicativo.

No TypeScript, `console.log` e variantes são transformados em chamadas ao objeto `console` do ambiente de execução. No navegador, elas acabam chamando métodos no `console` global. No Node.js, elas chamam métodos no objeto `console` global que escrevem para `process.stdout` e `process.stderr`.

## Ver Também:

A depuração é uma parte essencial do desenvolvimento de software. Aqui estão alguns recursos adicionais que você pode achar úteis:

- [Depuração no Visual Studio Code](https://code.visualstudio.com/docs/editor/debugging)
- [Trabalhando com o console](https://developer.mozilla.org/pt-BR/docs/Web/API/console)
- [Chrome DevTools](https://developers.google.com/web/tools/chrome-devtools) 
- [Node.js Debugger](https://nodejs.org/api/debugger.html)