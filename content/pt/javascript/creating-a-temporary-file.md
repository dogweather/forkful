---
title:    "Javascript: Criando um arquivo temporário"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Javascript?

Muitas vezes, criar um arquivo temporário é uma necessidade ao desenvolver um programa em Javascript. Isso pode ser necessário para armazenar informações temporárias ou dados que não precisam ser salvos permanentemente. Além disso, pode ser útil para evitar conflitos de nomes de arquivos ao trabalhar com vários usuários em um sistema compartilhado.

## Como criar um arquivo temporário em Javascript?

Para criar um arquivo temporário em Javascript, podemos usar a função "createFile" do módulo "fs". Veja o exemplo abaixo:

```Javascript
const fs = require('fs');

// Cria um arquivo temporário com nome aleatório
fs.createFile('tempFile.txt', (err, file) => {
  if (err) throw err;
  console.log('Arquivo temporário criado!');
});
```

Isso irá criar um arquivo chamado "tempFile.txt" no diretório em que o código está sendo executado.

Também podemos especificar um diretório no qual o arquivo temporário será criado, usando o seguinte código:

```Javascript
const fs = require('fs');
const path = require('path');

// Cria um arquivo temporário no diretório "temp"
const tempDir = path.join(__dirname, 'temp');
fs.createFile(path.join(tempDir, 'tempFile.txt'), (err, file) => {
  if (err) throw err;
  console.log('Arquivo temporário criado no diretório temp!');
});
```

A função "createFile" também nos permite especificar o conteúdo do arquivo temporário. Veja o exemplo abaixo:

```Javascript
const fs = require('fs');

// Cria um arquivo temporário com conteúdo "Hello World!"
fs.createFile('tempFile.txt', 'Hello World!', (err, file) => {
  if (err) throw err;
  console.log('Arquivo temporário criado com sucesso!');
});
```

## Aprofundando-se na criação de arquivos temporários

Além da função "createFile", existem outras maneiras de criar um arquivo temporário em Javascript. Algumas bibliotecas, como o "temp" e o "tmp", oferecem métodos mais avançados para gerenciar arquivos temporários.

Também é importante lembrar que precisamos nos certificar de excluir o arquivo temporário após seu uso para evitar ocupar espaço desnecessariamente no sistema. Podemos fazer isso usando a função "unlink" do módulo "fs".

Veja um exemplo de como criar um arquivo temporário usando a biblioteca "temp" e excluí-lo após seu uso:

```Javascript
const temp = require('temp').track(); // Inicializa a biblioteca "temp"

// Cria um arquivo temporário com conteúdo "Hello World!"
let tempPath = temp.path({prefix: 'temp', suffix: '.txt'});
fs.writeFileSync(tempPath, 'Hello World!');

// Exibe o conteúdo do arquivo temporário
console.log(fs.readFileSync(tempPath, 'utf8'));

// Exclui o arquivo temporário
temp.cleanupSync();
```

Ao finalizar o uso do arquivo temporário, a função "cleanupSync" da biblioteca "temp" é responsável por excluí-lo do sistema.

## Veja também

- Documentação oficial do Node.js para a função "createFile": https://nodejs.org/api/fs.html#fs_fs_createfile_path_data_callback
- Documentação oficial da biblioteca "temp": https://github.com/bruce/node-temp
- Documentação oficial da biblioteca "tmp": https://github.com/raszi/node-tmp