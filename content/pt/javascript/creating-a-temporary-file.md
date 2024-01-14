---
title:                "Javascript: Criando um arquivo temporário"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário?

A criação de arquivos temporários é um processo comumente utilizado em programação para armazenar informações temporárias durante a execução de um programa. Isso pode ser útil para armazenar dados que não precisam de uma permanência prolongada e podem ser excluídos após o uso.

## Como criar um arquivo temporário em Javascript

A criação de um arquivo temporário em Javascript é feita utilizando a função ```fs.mktempsync()```. Essa função recebe dois parâmetros, o primeiro é o prefixo do nome do arquivo e o segundo é uma callback contendo o nome e o caminho do arquivo criado. Veja um exemplo abaixo:

```Javascript 
const fs = require('fs');
const tempFile = fs.mktempsync('tempFile');
console.log(tempFile.name); // nome do arquivo temporário
console.log(tempFile.path); // caminho do arquivo criado
```

Ao executar este código, um arquivo temporário será criado com um nome aleatório começando com o prefixo "tempFile" e sua localização será retornada através da callback.

## Aprofundando na criação de arquivos temporários

É importante lembrar que arquivos temporários são excluídos automaticamente após o término do programa em que foram criados. No entanto, para garantir que o arquivo seja excluído assim que não for mais necessário, é possível utilizar a função ```fs.unlinkSync()```. Essa função recebe como parâmetro o caminho do arquivo que deseja excluir, como no exemplo abaixo:

```Javascript 
const fs = require('fs');
fs.unlinkSync(tempFile.path); // excluindo o arquivo temporário criado anteriormente
```

Também é importante ter cuidado com o uso de arquivos temporários em ambientes de produção, pois eles podem acumular e prejudicar a performance do sistema. Por isso, é recomendável utilizar essa técnica apenas quando necessário e sempre garantir a exclusão dos arquivos após o uso.

## Veja também

- [Documentação oficial do Node.js sobre a função ```fs.mktempsync()```](https://nodejs.org/api/fs.html#fs_fs_mkstemp_sync_prefix)
- [Tutorial sobre a criação de arquivos temporários em diversas linguagens de programação](https://www.tutorialspoint.com/create-temporary-files-in-various-languages)
- [Artigo sobre o uso de arquivos temporários em ambientes de produção](https://blog.rescale.com/performance-considerations-when-creating-a-temporary-file/)