---
title:                "Escrevendo um arquivo de texto"
html_title:           "Javascript: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto?

Existem diversas razões pelas quais alguém pode querer escrever um arquivo de texto usando Javascript. Uma delas é a necessidade de armazenar dados que podem ser acessados e modificados posteriormente, como informações de usuário ou configurações de aplicativos.

## Como fazer

Para escrever um arquivo de texto utilizando Javascript, podemos utilizar o objeto File System (Sistema de Arquivos) disponível através do módulo "fs" do Node.js. Para isso, podemos seguir os seguintes passos:

1. Importar o módulo "fs" utilizando a função `require`.
2. Utilizar o método `writeFile` do objeto File System para escrever o conteúdo do arquivo.
3. Passar como parâmetros o nome do arquivo, o conteúdo e uma função de callback para lidar com possíveis erros.

Um exemplo de código seria:

```Javascript
 const fs = require('fs');

 fs.writeFile('meu_arquivo.txt', 'Olá, mundo!', (err) => {
   if (err) throw err;
   console.log('Arquivo foi escrito com sucesso!');
 });
```

Isso irá criar um arquivo chamado "meu_arquivo.txt" com o conteúdo "Olá, mundo!". Caso o arquivo já exista, ele será sobrescrito.

## Aprofundando

Se desejarmos escrever conteúdo em um arquivo de forma assíncrona, podemos utilizar o método `write` do objeto File System ao invés do `writeFile`. Isso nos permite escrever em pedaços menores, o que pode ser útil para lidar com arquivos grandes.

Além disso, podemos utilizar opções como `flag` para especificar o modo de escrita (por exemplo, adicionar novos dados no final do arquivo ao invés de sobrescrever) e `encoding` para especificar a codificação dos caracteres.

## Veja também

- [Documentação do módulo "fs" do Node.js](https://nodejs.org/api/fs.html)
- [Tutorial de escrita de arquivos com Node.js](https://www.digitalocean.com/community/tutorials/how-to-write-files-in-node-js)