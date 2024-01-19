---
title:                "Verificando se um diretório existe"
html_title:           "Javascript: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O Quê e Por Quê?

Verificar se um diretório existe é uma das funções básicas em programação, que confirma a existência de um diretório específico em um sistema de arquivos. Isso é útil pois evita erros durante a manipulação do sistema de arquivos.

## Como fazer:

Nós podemos verificar se um diretório existe usando o módulo 'fs' do Node.js. Aqui está um exemplo simples:

```Javascript
const fs = require('fs');

fs.access('teste-diretório', (err) => {
  if (err) {
    console.log('O diretório não existe');
  } else {
    console.log('O diretório existe');
  }
});
```
Neste caso, você verá "O diretório não existe" se o diretório 'teste-diretório' não existir, ou "O diretório existe" se existir.

## Mergulho Profundo

Historicamente, a necessidade de verificar se um diretório existe decorre da possibilidade de erros de E/S durante a manipulação de sistemas de arquivos. No passado, usávamos o método 'fs.exists', mas foi depreciado devido à sua incapacidade de diferenciar entre erros de E/S e a inexistência do caminho especificado.

Uma alternativa é o método fs.existsSync(), que retorna um valor booleano indicando a existência ou não de um diretório. No entanto, este método é sincronizado, o que pode bloquear outras operações. Por isso, é recomendado o uso do método 'fs.access' para operações assíncronas.

Posteriormente, com a popularização do módulo 'fs-extra' do Node.js, passamos a ter acesso a funções simplificadas para manipulação de diretórios, como o método 'ensureDir()' que cria um diretório se ele não existir.

## Veja Also 

[Documentação do Node.js 'fs' Module](https://nodejs.org/api/fs.html)

['fs-extra' Módulo no npm](https://www.npmjs.com/package/fs-extra)

[Artigos sobre manipulação de sistema de arquivos](https://www.digitalocean.com/community/tutorial_series/working-with-data-and-files-in-node-js).