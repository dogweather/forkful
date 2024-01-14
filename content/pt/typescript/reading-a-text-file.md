---
title:                "TypeScript: Lendo um arquivo de texto"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto em TypeScript?

Ler um arquivo de texto pode ser uma tarefa comum em muitos projetos de programação, especialmente quando precisamos lidar com dados externos. Ao aprender como ler um arquivo de texto em TypeScript, você terá uma ferramenta útil para incorporar em seus codigos e manipular informações diversas.

## Como fazer:

Para ler um arquivo de texto em TypeScript, siga os seguintes passos:

1. Abra o arquivo usando a função `createReadStream` do módulo `fs`. Isso irá criar uma stream de leitura do arquivo.

2. Configure o encoding para `utf-8` para garantir que os caracteres sejam lidos corretamente.

3. Utilize `on("data")` para ler os dados do arquivo e armazená-los em uma variável.

4. Ao final da leitura, exiba a variável com os dados do arquivo usando `console.log()`.

Veja um exemplo de código que realiza esses passos:

```TypeScript
import * as fs from "fs"; // importa o módulo fs para manipular arquivos

const lerArquivo = (caminho: string) => {
  const stream = fs.createReadStream(caminho, { encoding: "utf-8" }); // abre o arquivo usando createReadStream e define o encoding
  let dados = ""; // variável para armazenar os dados do arquivo

  stream.on("data", (chunk) => {
    dados += chunk; // armazena os dados lidos do arquivo
  });

  stream.on("end", () => {
    console.log(dados); // exibe os dados do arquivo lido
  });
};

lerArquivo("caminho/para/o/arquivo.txt"); // chamada da função com o caminho do arquivo
```

Ao executar esse código, você deve receber a saída no console com os dados do arquivo:

```
Olá, leitores!

Este é um arquivo de texto de exemplo.

Esperamos que esse tutorial seja útil para você aprender a ler arquivos de texto em TypeScript.

Até a próxima!
```

## Mergulhando mais fundo:

Além da função `createReadStream`, o módulo `fs` também possui outras opções para ler arquivos, como as funções `readFile` e `readFileSync`. Cada uma dessas opções possui suas próprias peculiaridades e cabe a você escolher qual é a mais adequada para o seu projeto.

Outra coisa importante a se mencionar é sobre o tratamento de erros ao lidar com arquivos. É sempre importante incluir um tratamento de erro em seu código para lidar com possíveis falhas ao ler um arquivo, por exemplo, se o arquivo não existir ou não tiver permissão de leitura.

## Veja também:

- [Documentação oficial do TypeScript](https://www.typescriptlang.org/)
- [Guia para ler arquivos em TypeScript](https://www.pluralsight.com/guides/node-js/file-handling-in-typescript)
- [Tutorial de manipulação de arquivos em TypeScript](https://codeburst.io/file-handling-in-typescript-part-1-b97ae89a70e1)