---
title:                "TypeScript: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário no TypeScript?

Criar arquivos temporários é uma tarefa comum em muitos projetos de desenvolvimento de software. Esses arquivos são usados para armazenar informações temporárias ou para criar backups de dados. No TypeScript, podemos criar facilmente arquivos temporários para facilitar essas tarefas.

## Como criar um arquivo temporário no TypeScript

Para criar um arquivo temporário no TypeScript, precisamos utilizar a biblioteca nativa 'fs' (file system). Com ela, podemos usar a função 'mkdtempSync' para criar o diretório temporário e a função 'writeFileSync' para salvar informações dentro do arquivo. Veja o exemplo abaixo:

```TypeScript
import fs from 'fs';

const temporaryDir = fs.mkdtempSync('temp-'); // cria um diretório temporário
const temporaryFile = `${temporaryDir}/temp_file.txt`; // cria um caminho para o arquivo temporário

fs.writeFileSync(temporaryFile, 'Este é um arquivo temporário.'); // escreve informações no arquivo

console.log(`Arquivo temporário criado: ${temporaryFile}`);
```

Output:
```
Arquivo temporário criado: /temp-x6lR1/temp_file.txt
```

## Mergulho profundo na criação de arquivos temporários

Além das funções mencionadas acima, podemos adicionar alguns parâmetros extras para personalizar ainda mais a criação dos arquivos temporários. Por exemplo, com a função 'mkdtempSync', podemos definir um prefixo e/ou sufixo para o nome do diretório.

```TypeScript
const temporaryDir = fs.mkdtempSync('my_app_temp-', { encoding: 'utf8' }); // cria um diretório temporário com um prefixo personalizado
```

Já com a função 'writeFileSync', podemos adicionar opções de codificação e permissões para o arquivo que será criado.

```TypeScript
fs.writeFileSync(temporaryFile, 'Este é um arquivo temporário.', { encoding: 'utf8', mode: 0o644 }); // escreve informações no arquivo com codificação UTF-8 e permissões de leitura e escrita
```

Esses são apenas alguns exemplos de como podemos personalizar a criação de arquivos temporários no TypeScript. É importante lembrar que, após o uso, devemos excluir esses arquivos temporários para evitar o acúmulo de dados desnecessários em nosso sistema.

## Veja também

- [Documentação do Node.js sobre criação de arquivos temporários](https://nodejs.org/api/fs.html#fs_fs_mkdtempsync_prefix_options)
- [Tutorial sobre criação de arquivos temporários em TypeScript](https://www.digitalocean.com/community/tutorials/how-to-create-temporary-files-and-directories-in-node-js-pt)
- [Exemplo de uso da função 'mkdtempSync'](https://dev.to/softchris/temporary-local-file-system-folders-in-nodejs-n7k)