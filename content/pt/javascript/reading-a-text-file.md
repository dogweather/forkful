---
title:                "Lendo um arquivo de texto"
html_title:           "Bash: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Lendo um arquivo de texto com JavaScript

## O que & Por quê?

Ler um arquivo de texto é o processo de acessar e interpretar os dados armazenados em um arquivo de texto. Programadores fazem isso para conseguirem importar e manipular dados de maneira eficiente e rápida.

## Como:

Para ler um arquivo de texto em JavaScript, você pode usar o pacote fs (file system) do Node.js. Aqui está um exemplo simples:

```Javascript
const fs = require('fs');

fs.readFile('caminhoDoSeuArquivo.txt', 'utf8', (erro, dados) => {
  if (erro) {
    console.error('Deu erro na leitura!', erro)
  } else {
    console.log(dados);
  }
});
```
Quando executar este código, será impresso no console o conteúdo do seu arquivo de texto.

## Um Mergulho Mais Profundo

Historicamente, a leitura de arquivos de texto é uma funcionalidade fundamental de muitos programas. Python, por exemplo, faz isso muito bem.

Existem diversas maneiras alternativas para ler um arquivo de texto em JavaScript, uma delas é o método 'readFileSync' que ler o arquivo de forma síncrona. 

```Javascript
const dados = fs.readFileSync('caminhoDoSeuArquivo.txt', 'utf8');
console.log(dados);
```
Mas é importante mencionar que a leitura síncrona bloqueia o processo até que a leitura esteja concluída, o que pode ser um problema para arquivos grandes.

Detalhes de implementação: quando você abre o arquivo com 'readFile' ou 'readFileSync', o node.js lê o arquivo inteiro de uma vez só na memória. Isso não é um problema para arquivos pequenos. Entretanto, para arquivos maiores, pode ser mais eficiente ler partes do arquivo de cada vez. Para isso, podemos usar o método 'createReadStream'.

## Veja também:

Para mais informações sobre como ler um arquivo de texto com JavaScript, você pode usar os seguintes links:

- Documentação oficial do Node.js: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- StackOverflow: [stackoverflow.com/questions/tagged/javascript](https://stackoverflow.com/questions/tagged/javascript)