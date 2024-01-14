---
title:    "Javascript: Criando um arquivo temporário"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar arquivos temporários em Javascript?

Muitas vezes, quando estamos programando em Javascript, precisamos armazenar dados temporários durante a execução de um programa. Esses arquivos temporários podem ser úteis para armazenar informações temporárias, manipular arquivos ou executar operações que precisam de um local de armazenamento temporário. Neste artigo, vamos explorar como criar e usar arquivos temporários em Javascript.

## Como fazer:

Para criar um arquivo temporário em Javascript, podemos usar a função `fs.mkdtemp` do módulo `fs` do Node.js. Esta função cria um diretório temporário e retorna o caminho para este diretório. Podemos então usar este caminho para manipular o arquivo temporário da forma desejada.

```Javascript
const fs = require('fs');

// Cria um diretório temporário com o prefixo 'temp-'
fs.mkdtemp('temp-', (err, folder) => {
  if (err) throw err;

  // Caminho para o arquivo temporário criado
  const tempFilePath = `${folder}/tempFile.txt`;

  // Escreve texto no arquivo temporário
  fs.writeFile(tempFilePath, 'Este é um arquivo temporário', (err) => {
    if (err) throw err;

    console.log('Arquivo temporário criado com sucesso!');
  });
});
```

Ao executar o código acima, um arquivo temporário com o nome `tempFile.txt` será criado na pasta especificada. Podemos realizar diversas operações neste arquivo, como escrever e ler informações. Ao final da execução do nosso programa, o arquivo temporário será excluído automaticamente.

## Uma análise mais aprofundada:

Além da função `fs.mkdtemp`, o módulo `fs` também possui outras funções úteis para o trabalho com arquivos temporários em Javascript, como `fs.mkstemp` e `fs.close`. Além disso, podemos definir opções adicionais para a criação do arquivo temporário, como o prefixo e sufixo do nome do arquivo, através do segundo parâmetro da função `fs.mkdtemp`.

É importante lembrar que os arquivos temporários são apenas temporários e não devem ser usados para armazenar informações sensíveis ou importantes. Além disso, é responsabilidade do desenvolvedor garantir que o arquivo temporário seja excluído após o uso para evitar possíveis problemas de segurança.

## Veja também:

- Documentação do módulo `fs` do Node.js: https://nodejs.org/api/fs.html
- Exemplo de criação de arquivos temporários com Node.js: https://stackabuse.com/creating-and-deleting-temporary-files-in-node-js/