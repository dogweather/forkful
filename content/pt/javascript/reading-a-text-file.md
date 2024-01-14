---
title:                "Javascript: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto em Javascript?

Existem muitos cenários em que pode ser útil ler um arquivo de texto em uma aplicação Javascript. Você pode precisar processar dados armazenados em um arquivo ou pode querer criar um sistema de log para rastrear as ações de usuários em seu site. Independentemente do motivo, saber como ler um arquivo de texto pode ser uma habilidade útil para programadores.

## Como ler um arquivo de texto em Javascript

Para ler um arquivo de texto em Javascript, é necessário primeiro acessar o sistema de arquivos do seu computador usando a API de arquivos do navegador. Em seguida, você pode usar a função `readFile()` para obter o conteúdo do arquivo e armazená-lo em uma variável. Veja um exemplo de código abaixo:

```Javascript
const fs = require('fs');
fs.readFile('arquivo.txt', 'utf-8', (err, data) => {
  if (err) {
    console.error(err);
    return
  }
  console.log(data);
});
```

Neste exemplo, estamos usando o módulo `fs` para acessar o sistema de arquivos e a função `readFile()` para ler o arquivo com o nome de `arquivo.txt`. Certifique-se de substituir este nome pelo caminho e nome do seu arquivo de texto. É importante também fornecer o `utf-8` como segundo parâmetro para indicar a codificação do texto. A função `readFile()` retorna uma string contendo o conteúdo do arquivo, que é então impresso no console.

## Mergulho profundo na leitura de arquivos de texto em Javascript

Existem muitas outras maneiras de ler arquivos de texto em Javascript, incluindo o uso de bibliotecas de terceiros, como o `readline` e o `fstream`. Além disso, é importante estar ciente de que o processo de leitura de arquivos pode variar dependendo do ambiente de execução do seu código.

Para manipular grandes arquivos, é recomendável usar streams, que leem o arquivo em pequenos pedaços, em vez de lê-lo inteiramente e armazená-lo em memória. Além disso, é importante sempre tratar possíveis erros durante o processo de leitura de arquivos, para garantir que sua aplicação seja robusta e confiável.

## Veja também

- [Documentação do módulo `fs`](https://nodejs.org/docs/latest-v14.x/api/fs.html)
- [Tutorial sobre leitura de arquivos em Javascript](https://www.tutorialspoint.com/nodejs/nodejs_file_system.htm)
- [Artigo sobre streams em Javascript](https://medium.com/@marinaspas/working-with-streams-in-node-js-6308acbf1c00)