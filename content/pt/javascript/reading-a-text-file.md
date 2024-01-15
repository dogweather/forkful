---
title:                "Lendo um arquivo de texto"
html_title:           "Javascript: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto?

Ler arquivos de texto é uma tarefa comum na programação, especialmente em linguagens como o Javascript. Arquivos de texto contêm informações importantes e úteis que podem ser lidas pelo seu código, o que torna essa habilidade essencial para um programador.

## Como ler um arquivo de texto em Javascript

Para ler um arquivo de texto em Javascript, primeiro precisamos criar uma instância de um objeto responsável pela leitura de arquivos. Em seguida, utilizamos o método `readFile()` para ler o conteúdo do arquivo. Vejamos um exemplo abaixo:

```Javascript
// Importar o módulo "fs" para manipulação de arquivos
const fs = require('fs');

// Diretório e nome do arquivo a ser lido
const fileName = 'meuArquivo.txt';

// Instanciar um objeto para leitura de arquivos
const leitor = fs.createReadStream(fileName);

// Utilizar o método "readFile()" para ler o conteúdo do arquivo
fs.readFile(fileName, 'utf8', (err, data) => {
  // Se ocorrer um erro, exibir a mensagem
  if (err) console.log(err);

  // Caso contrário, exibir o conteúdo do arquivo
  console.log(data);
});
```

Ao executar esse código, o conteúdo do arquivo `meuArquivo.txt` será exibido no console. Você pode substituir o nome do arquivo por qualquer outro que desejar. Lembre-se sempre de utilizar os métodos `createReadStream()` e `readFile()` para ler arquivos de texto em Javascript.

## Mergulho Profundo

Além do método `readFile()`, o módulo `fs` também possui outras opções para ler arquivos de texto, como o `readFileSync()` e o `createReadStream()`, que funcionam de maneira um pouco diferente. Além disso, é possível adicionar opções como o tipo de codificação e o tamanho do buffer ao ler um arquivo. Recomendamos que você explore as diferentes opções e entenda qual é a mais adequada para o seu projeto.

## Veja também

- [Documentação oficial do Node.js sobre o módulo "fs"](https://nodejs.org/api/fs.html)
- [Artigo sobre leitura de arquivos em Javascript no Medium](https://medium.com/@thilinamb/javascript-fundamentals-how-to-read-files-in-nodejs-86efd7e118)
- [Tutorial sobre leitura e escrita de arquivos de texto em Javascript](https://www.digitalocean.com/community/tutorials/node-js-criando-e-lendo-arquivos)