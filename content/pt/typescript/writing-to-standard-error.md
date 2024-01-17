---
title:                "Escrevendo no erro padrão"
html_title:           "TypeScript: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O que e por que?

Escrever para o erro padrão é uma técnica utilizada pelos programadores para informar mensagens de erro ou outras informações relevantes durante a execução de um programa. Essas mensagens podem ser úteis para ajudar os programadores a identificar e corrigir problemas em seu código. Além disso, escrever para o erro padrão também pode ser útil durante a fase de depuração de um programa.

## Como fazer:

```TypeScript
console.error("Mensagem de erro");
```

Este é um exemplo simples de como escrever para o erro padrão em TypeScript. A mensagem "Mensagem de erro" será exibida no console como um erro. Você também pode adicionar variáveis ou outros dados à mensagem entre os parênteses, para fornecer informações mais específicas sobre o erro.

## Profundando:

Na história da programação, escrever para o erro padrão foi uma técnica usada para lidar com erros e depurar programas antes do uso de ferramentas de depuração modernas. Alternativamente, os programadores podem optar por escrever para o erro padrão como uma maneira de registrar informações importantes durante a execução de um programa sem interrompê-lo.

Os programadores também podem escrever para um arquivo de log em vez do erro padrão, o que pode ser útil para registrar informações de depuração em aplicativos de produção.

Na implementação detalhada de escrever para o erro padrão em TypeScript, é importante notar que a diferença entre `console.log()` e `console.error()` é a formatação e a aparência das mensagens no console. Enquanto `console.log()` é usado para fins de registro e depuração geral, o `console.error()` sinaliza um erro mais grave ou crítico.

## Veja também:

- [Documentação oficial do TypeScript](https://www.typescriptlang.org/docs/)
- [Artigo sobre como usar o console em JavaScript](https://developer.mozilla.org/pt-BR/docs/Web/API/Console)
- [Guia de depuração para iniciantes em TypeScript](https://blog.bitsrc.io/a-beginners-guide-to-debugging-typescript-6379ede98c1)