---
title:                "Javascript: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Javascript?

Criar um arquivo temporário pode ser útil em diversas situações de programação, como por exemplo, para armazenar dados que serão usados temporariamente e não precisam ser mantidos permanentemente no computador.

## Como criar um arquivo temporário em Javascript

Para criar um arquivo temporário em Javascript, podemos utilizar a função `fs.mkstemp()` do módulo "fs" nativo do Node.js. Esta função recebe dois parâmetros: o prefixo do nome do arquivo temporário e uma função de callback que será executada após a criação do arquivo.

```Javascript
const fs = require('fs');

// Criando um arquivo temporário com o prefixo "dados"
fs.mkstemp('dados-', (error, file) => {
   // Caso ocorra um erro na criação do arquivo, retornamos uma mensagem de erro
   if (error) {
      console.log(`Ocorreu um erro: ${error}`);
   } else {
      // Caso contrário, exibimos o nome do arquivo criado
      console.log(`Arquivo temporário criado com sucesso: ${file}`);
   }
});
```

Ao executar este código, teremos um arquivo temporário criado com um nome aleatório, iniciando com o prefixo "dados-". Podemos utilizar este arquivo para armazenar os dados que precisamos temporariamente.

## Aprofundando-se na criação de um arquivo temporário

Além da função `fs.mkstemp()`, o Node.js também possui outras formas de criar arquivos temporários, como o método `fs.createWriteStream()` e o módulo externo "temp". Cada uma dessas opções possui suas particularidades e pode ser mais adequada a determinadas situações.

Além disso, cabe ressaltar que é importante remover o arquivo temporário após o uso, para evitar sobrecarga de espaço em disco ou possíveis erros na execução do código. Para isso, podemos utilizar a função `fs.unlink()` passando como parâmetro o nome do arquivo temporário criado.

## Veja também

- Documentação oficial do Node.js sobre a função `fs.mkstemp()`: https://nodejs.org/api/fs.html#fs_fs_mkstemp_prefix_callback
- Tutorial sobre como criar arquivos temporários em Javascript: https://www.geeksforgeeks.org/how-to-create-temporary-file-in-node-js/
- Módulo "temp" do Node.js: https://www.npmjs.com/package/temp