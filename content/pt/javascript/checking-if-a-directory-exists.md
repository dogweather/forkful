---
title:    "Javascript: Verificando se um diretório existe"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que

Quando estamos trabalhando em um projeto de programação, muitas vezes precisamos verificar se um diretório já existe antes de criar um novo. Isso é importante porque garante que não estamos sobrepondo ou excluindo acidentalmente um diretório que pode conter arquivos importantes. Neste post, vamos aprender como verificar se um diretório existe usando o Javascript.

## Como fazer

Para verificar se um diretório existe em Javascript, podemos usar o módulo de sistema "fs" (file system). Este módulo contém diversas funções para trabalhar com arquivos e diretórios.

Primeiro, precisamos importar o módulo usando o comando `require`:

```Javascript
const fs = require('fs');
```

Em seguida, podemos usar a função `fs.existsSync()` para verificar se o diretório existe. Esta função recebe como parâmetro o caminho do diretório que queremos verificar e retorna um valor booleano (verdadeiro ou falso).

Por exemplo, vamos verificar se o diretório "meus_documentos" existe no diretório atual:

```Javascript
const dirPath = './meus_documentos';
const exists = fs.existsSync(dirPath);

// O valor de exists será true se o diretório existir, e false caso contrário
console.log(exists); 
// Output: true
```

Caso o diretório não exista, o valor de `exists` será falso.

## Profundidade

A função `fs.existsSync()` é síncrona, o que significa que ela irá bloquear a execução do código até que a verificação seja concluída. Isso pode ser problemático em casos onde precisamos lidar com operações de IO (input/output) que podem levar tempo.

Para contornar essa situação, podemos usar uma versão assíncrona da função, chamada `fs.access()`. Esta função recebe como parâmetro o caminho do diretório e uma callback que será executada assim que a verificação for concluída.

Vamos ver um exemplo:

```Javascript
const dirPath = './meus_documentos';
fs.access(dirPath, (error) => {
  if (error) {
    // O diretório não existe
    console.log("O diretório não existe.");
  } else {
    // O diretório existe
    console.log("O diretório existe.");
  }
});
```
Dessa forma, o código não será bloqueado enquanto a verificação estiver em andamento.

## Veja também

- Documentação oficial do módulo "fs" (em inglês): https://nodejs.org/api/fs.html
- Artigo sobre como lidar com arquivos assincronamente em Node.js (em inglês): https://blog.risingstack.com/working-with-files-using-node-js/
- Tutorial sobre como verificar se um arquivo ou diretório existe em Node.js (em inglês): https://stackabuse.com/file-handling-in-node-js/