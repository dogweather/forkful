---
title:                "Escrevendo um arquivo de texto"
date:                  2024-01-19
html_title:           "Arduino: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Escrever um arquivo de texto em JavaScript significa gravar dados em um arquivo no sistema do usuário. Programadores fazem isso para persistir informações entre sessões, exportar dados para serem usados em outro lugar ou simplesmente logar informações para debugging ou relatórios.

## Como Fazer:

Para escrever em um arquivo de texto, você pode usar a API `fs` no Node.js. Aqui está um código de exemplo.

```Javascript
const fs = require('fs');

let conteudo = "Olá, esse é um teste de escrita em arquivo";

fs.writeFile('exemplo.txt', conteudo, (err) => {
    if(err) throw err;
    console.log('O arquivo foi criado e salvo com sucesso!');
});
```

Saída:

```
O arquivo foi criado e salvo com sucesso!
```

## Mergulho Profundo:

A escrita e leitura de arquivos em JavaScript era tradicionalmente realizada somente no lado do servidor, usando Node.js, devido a restrições de segurança no navegador. Contudo, com as modernas APIs web, como a File System Access API, agora é possível ler e escrever arquivos também no lado do cliente. Alternativas para salvar dados incluem bancos de dados, armazenamento em nuvem ou localStorage para aplicações web. A implementação mais básica no servidor ainda usa a biblioteca `fs`, que vem embutida no Node.js e maneja leitura e escrita de arquivos de forma síncrona ou assíncrona.

## Ver Também:

- Documentação da API `fs` do Node.js: https://nodejs.org/api/fs.html
- Introdução à File System Access API para o navegador: https://web.dev/file-system-access/
- MDN Web Docs – Armazenamento local: https://developer.mozilla.org/pt-BR/docs/Web/API/Window/localStorage
