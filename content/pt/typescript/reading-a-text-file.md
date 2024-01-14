---
title:    "TypeScript: Lendo um arquivo de texto"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto?

Ler arquivos de texto é uma tarefa comum em muitas aplicações de programação. Ao aprender como fazer isso em TypeScript, você pode expandir suas habilidades e tornar seus códigos mais eficientes. Além disso, é uma forma de acessar informações armazenadas em arquivos de texto, como configurações ou dados de usuários.

## Como Fazer

Para ler um arquivo de texto em TypeScript, você precisará importar o módulo "fs" (file system) do Node.js e utilizar o método "readFileSync()". Veja um exemplo abaixo:

```TypeScript
import * as fs from 'fs';

let arquivoTexto = fs.readFileSync('caminho/do/arquivo.txt', 'utf-8');
console.log(arquivoTexto);
```

O código acima irá ler o arquivo de texto com o caminho especificado e armazená-lo na variável "arquivoTexto". Em seguida, utilizando o método "console.log()", o conteúdo do arquivo será impresso no console.

## Mergulho Profundo

Ao utilizar o método "readFileSync()", é importante definir o formato de codificação correto do arquivo que está sendo lido. No exemplo acima, utilizamos o formato 'utf-8', mas dependendo do arquivo, pode ser necessário utilizar outros formatos como 'ascii' ou 'latin1'.

Além disso, também é possível utilizar o método "readFile()" que é assíncrono, ou seja, ele não bloqueia a execução do código enquanto o arquivo é lido. Isso pode ser útil em casos em que o arquivo é grande e pode levar mais tempo para ser lido.

## Veja Também

- [Documentação do módulo "fs" do Node.js](https://nodejs.org/api/fs.html)
- [Tutorial sobre como ler arquivos de texto em TypeScript](https://www.digitalocean.com/community/tutorials/nodejs-reading-files-with-fs)
- [Exemplos de leitura de arquivos de texto com TypeScript](https://github.com/thecodeholic/nodejs-fs-example)