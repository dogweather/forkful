---
title:                "Criando um arquivo temporário"
html_title:           "Javascript: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O que é e por que fazer?

Criar um arquivo temporário refere-se a criar um arquivo que é usado por um curto período de tempo, geralmente durante a execução de um programa. Os programadores usam esse recurso para armazenar dados temporários que serão usados no decorrer do programa.

## Como fazer:

```Javascript
// Importar o módulo nativo 'fs'
const fs = require('fs');

// Usar o método 'writeFile' para criar um arquivo temporário 
fs.writeFile('/path/to/file', 'conteúdo', (err) => {
  // Executado após o arquivo ser criado
  if (err) throw err;
  console.log('Arquivo temporário criado com sucesso!');
});
```

## Detalhando mais:

Existem várias razões pelas quais os programadores podem precisar criar um arquivo temporário. Por exemplo, durante a execução de um programa, pode ser necessário armazenar dados temporários, como resultados intermediários, que podem ser excluídos após o término do programa. Outra razão é quando um programa precisa acessar um arquivo temporário de outro programa.

Além de usar o método `writeFile` do módulo `fs`, também é possível criar um arquivo temporário usando outros módulos de terceiros, como o `tempfile`. Além disso, existem outras abordagens para a criação de arquivos temporários que podem ser mais adequadas para determinados casos, como a criação de arquivos temporários em memória.

## Veja também:

- Documentação do método `writeFile` do módulo `fs`: https://nodejs.org/docs/latest/api/fs.html#fs_fs_writefile_file_data_options_callback
- Documentação do módulo `tempfile`: https://www.npmjs.com/package/tempfile