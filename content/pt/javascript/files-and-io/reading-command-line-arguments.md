---
date: 2024-01-20 17:56:15.006828-07:00
description: "Ler argumentos da linha de comando em JavaScript significa pegar informa\xE7\
  \xF5es extras passadas ao iniciar um script Node.js. Programadores fazem isso para\u2026"
lastmod: '2024-03-13T22:44:46.977575-06:00'
model: gpt-4-1106-preview
summary: "Ler argumentos da linha de comando em JavaScript significa pegar informa\xE7\
  \xF5es extras passadas ao iniciar um script Node.js. Programadores fazem isso para\u2026"
title: Lendo argumentos da linha de comando
---

{{< edit_this_page >}}

## O Que é & Por Que?
Ler argumentos da linha de comando em JavaScript significa pegar informações extras passadas ao iniciar um script Node.js. Programadores fazem isso para personalizar a execução de um programa sem alterar o código.

## Como Fazer:
```javascript
// myScript.js
process.argv.forEach((val, index) => {
  console.log(`${index}: ${val}`);
});

// Rodando o script no terminal
// $ node myScript.js arg1 arg2 arg3

/* Saída esperada:
0: /path/to/node
1: /path/to/myScript.js
2: arg1
3: arg2
4: arg3
*/
```

## Mergulho Profundo
Historicamente, acessar argumentos da linha de comando no Node.js sempre foi possível pelo objeto `process.argv`, um array que contém todos os argumentos passados. Alternativas modernas incluem bibliotecas como `yargs` ou `commander` que facilitam a parseação e adição de opções mais elaboradas. Os argumentos começam na terceira posição do array porque as duas primeiras são reservadas: a primeira para o caminho do executável do Node.js e a segunda para o script sendo executado.

## Ver Também:
- Documentação oficial do Node.js para `process.argv`: https://nodejs.org/docs/latest/api/process.html#process_process_argv
- `yargs` para parseamento mais avançado de argumentos: https://www.npmjs.com/package/yargs
- `commander` para construção de interfaces de linha de comando: https://www.npmjs.com/package/commander
