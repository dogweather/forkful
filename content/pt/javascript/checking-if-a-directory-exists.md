---
title:                "Verificação da existência de um diretório"
html_title:           "Javascript: Verificação da existência de um diretório"
simple_title:         "Verificação da existência de um diretório"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Por que verificar se um diretório existe em Javascript?

Verificar se um diretório existe em um projeto pode ser uma tarefa muito útil para garantir que o código funcione corretamente. Além disso, esta verificação é importante para evitar erros e falhas no sistema.

## Como fazer isso

Para verificar se um diretório existe, é necessário usar a função nativa do Javascript `fs.existsSync()`. Esta função retorna um valor booleano que indica se o diretório existe ou não.

Vejamos um exemplo prático:

```Javascript
const fs = require('fs'); // Importar o módulo fs
const directory = './meu_diretorio'; // Definir o diretório que desejamos verificar

if (fs.existsSync(directory)) { 
  // Verificar se o diretório existe
  console.log('O diretório existe!'); // Imprimir mensagem caso exista
} else {
  console.log('O diretório não existe!'); // Imprimir mensagem caso não exista
}
```

No exemplo acima, importamos o módulo `fs` para ter acesso às funções do sistema de arquivos. Em seguida, definimos o diretório que queremos verificar e utilizamos a função `existsSync()` para verificar se ele existe. Dependendo do resultado, é exibida uma mensagem de que o diretório existe ou não.

## Aprofundando

Além da função `fs.existsSync()`, existem outras maneiras de verificar se um diretório existe em Javascript. Uma delas é utilizando a biblioteca `fs-extra`, que possui a função `fs.pathExists()` que também retorna um valor booleano indicando a existência do diretório.

Também é possível utilizar a função `fs.lstatSync()`, que permite verificar o tipo do arquivo e, consequentemente, inferir se é um diretório ou não.

Caso seja necessário criar um diretório caso ele não exista, é possível utilizar a função `fs.mkdirSync()`, passando como parâmetro o caminho do diretório que desejamos criar.

## Veja também

- Documentação oficial do Javascript: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript
- Documentação do módulo fs do Node.js: https://nodejs.org/api/fs.html
- Documentação da biblioteca fs-extra: https://www.npmjs.com/package/fs-extra