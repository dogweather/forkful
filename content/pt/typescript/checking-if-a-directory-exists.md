---
title:                "TypeScript: Verificando se um diretório existe"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Verificar se um diretório existe é uma tarefa comum em programação. Isso é importante porque permite que o programa faça verificações e tome decisões com base na presença ou ausência de um diretório específico no sistema de arquivos.

## Como fazer

Verificar se um diretório existe em TypeScript é relativamente simples. Primeiro, precisamos importar o módulo "fs" (File System) para ter acesso às funções relacionadas ao sistema de arquivos.

```TypeScript
import * as fs from "fs";
```

Em seguida, podemos usar a função "existsSync" para verificar se um diretório existe no caminho especificado. Esta função retorna um valor booleano, true se o diretório existir e false se não existir.

```TypeScript
const directoryExists = fs.existsSync("caminho/do/diretório");
console.log(directoryExists); // Saída: true ou false
```

## Deep Dive

Ao verificar se um diretório existe, é importante entender o que pode interferir no resultado. Algumas coisas a serem consideradas são:

- Erros de permissão: se o programa não tiver permissão para acessar o diretório, a função "existsSync" retornará false.
- Caminho absoluto vs caminho relativo: é importante fornecer o caminho correto para o diretório. Se o caminho for relativo, ele será resolvido em relação à localização do arquivo TypeScript. Se for absoluto, ele deve ser fornecido da forma exata.
- Erros de digitação: verifique se o caminho fornecido está correto e livre de erros de digitação, pois isso pode resultar em um resultado inesperado.

## Veja também

- [Documentação do módulo fs](https://nodejs.org/api/fs.html)
- [Tutorial: Como usar o módulo fs em TypeScript](https://www.digitalocean.com/community/tutorials/nodejs-fs-module-typescript)
- [Artigo: Sistema de arquivos em Node.js](https://www.luiztools.com.br/post/tutorial-de-node-js-sistema-de-arquivos/)