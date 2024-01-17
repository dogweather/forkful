---
title:                "Criando um arquivo temporário"
html_title:           "TypeScript: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O que & Por que?

Criar um arquivo temporário é uma prática comum entre os programadores, que consiste em criar um arquivo temporário para armazenar dados temporários durante a execução de um programa. Isso é útil em situações em que o programa precisa armazenar informações temporárias e não é necessário salvar essas informações permanentemente.

## Como fazer:

```TypeScript
import fs from 'fs';

// Cria um arquivo temporário com um nome aleatório e escreve "Hello World!" dentro dele
const tempFile = fs.mkdtempSync('temp-');
fs.writeFileSync(tempFile + '/temp.txt', 'Hello World!');

// Lê o conteúdo do arquivo e o imprime no console
const tempContent = fs.readFileSync(tempFile + '/temp.txt', 'utf8');
console.log(tempContent);

// Deleta o arquivo temporário
fs.unlinkSync(tempFile + '/temp.txt');
```

## Aprofundamento:

A criação de arquivos temporários é uma técnica amplamente utilizada em programas, especialmente em situações em que os dados temporários não precisam ser armazenados permanentemente e podem ser descartados após o uso. Alternativas para criar arquivos temporários incluem o uso de memória RAM ou bancos de dados temporários.

Na implementação do TypeScript, existem funções específicas do módulo ```fs``` do Node.js que permitem criar, ler e excluir arquivos temporários. Essas funções oferecem uma maneira simples e eficiente de gerenciar arquivos temporários em um programa.

## Veja também:

- [Documentação do módulo fs no Node.js](https://nodejs.org/api/fs.html)
- [Explicação sobre a criação de arquivos temporários em TypeScript](https://www.typescriptlang.org/docs/handbook/stdlib/file-system.html#tempfilenames)