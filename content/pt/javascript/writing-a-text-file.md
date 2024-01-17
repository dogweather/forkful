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

## O que é e por que os programadores escrevem arquivos de texto?

Escrever um arquivo de texto significa gravar informações em um arquivo que pode ser lido por seres humanos e computadores. Os programadores fazem isso para armazenar dados importantes, como configurações, logs ou informações de usuários.

## Como fazer:

```Javascript
// Criar um arquivo de texto
const fs = require('fs'); // Módulo para manipular arquivos
const text = "Este é um exemplo de texto que será gravado em um arquivo."; // Texto a ser gravado
fs.writeFile("arquivo.txt", text, function (err) { // Função para escrever o texto no arquivo
  if (err) {
    console.log(err); // Exibir erro, se houver
  } else {
    console.log("Arquivo criado e texto gravado com sucesso!"); // Exibir sucesso
  }
});

// Ler um arquivo de texto
fs.readFile("arquivo.txt", "utf8", function (err, data) { // Função para ler o arquivo
  if (err) {
    console.log(err); // Exibir erro, se houver
  } else {
    console.log(data); // Exibir conteúdo do arquivo
  }
});
```

## Aprofundando:

Escrever arquivos de texto é uma prática comum na programação, pois oferece uma forma de armazenar dados de maneira simples e legível. Antes de existirem bancos de dados ou outras formas de armazenamento de dados, os programadores costumavam gravar informações em arquivos de texto.

Existem outras formas de escrever e ler arquivos além do método mostrado acima. Por exemplo, o módulo "fs" também oferece as funções "appendFile" para adicionar conteúdo a um arquivo existente e "readFileSync" para ler um arquivo de forma síncrona. Além disso, existem outros módulos disponíveis que podem ser usados para escrever e manipular arquivos de texto.

## Veja também:

- [Documentação do módulo "fs" do Node.js] (https://nodejs.org/api/fs.html)
- [Manipulação de arquivos com Node.js - DevMedia] (https://www.devmedia.com.br/manipulando-arquivos-com-node-js/30652)
- [Importância dos arquivos de texto para programadores - Medium] (https://medium.com/@lucashcordeiro/a-import%C3%A2ncia-dos-arquivos-de-texto-para-os-programadores-e-porqu%C3%AA-o-texto-%C3%A9-o-estresse-da-biblioteca-of-d63b9ec95758)