---
title:                "Lendo argumentos de linha de comando"
html_title:           "TypeScript: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O que é e por que é importante?

Ler argumentos da linha de comando é uma habilidade fundamental para qualquer programador que trabalhe com linguagens compiladas, como TypeScript. Esta habilidade permite que você passe informações ao seu programa enquanto ele está rodando, para que ele possa tomar decisões e executar tarefas específicas de acordo com esses argumentos.

## Como fazer:

```TypeScript
// Exemplo de código para ler argumentos da linha de comando

// Importando o módulo "process" do Node.js
import * as process from 'process';

// Atribuindo os argumentos a uma variável
const args = process.argv;

// Imprimindo os argumentos para a saída do console
console.log(`Os argumentos foram: ${args}`);

// Resultado:
// Os argumentos foram: [node, index.ts, argumento1, argumento2, ...]
```

## Profundidade:

A leitura de argumentos da linha de comando não é uma técnica nova, ela é usada há décadas em várias linguagens de programação. No entanto, com o aumento da popularidade do Node.js e do uso do TypeScript para desenvolvimento web, essa habilidade se tornou ainda mais importante. Existem também várias bibliotecas e módulos disponíveis que podem facilitar ainda mais a leitura e manipulação de argumentos da linha de comando.

## Veja também:

- [Documentação oficial do TypeScript sobre o módulo "process"](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-0.html#the-process-return-is-gone)
- [Tutorial sobre como ler argumentos da linha de comando em TypeScript](https://stackabuse.com/reading-command-line-arguments-in-typescript/)