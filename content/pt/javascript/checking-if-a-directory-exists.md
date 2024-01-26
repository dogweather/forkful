---
title:                "Verificando se um diretório existe"
date:                  2024-01-20T14:57:17.684400-07:00
html_title:           "Fish Shell: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Verificar a existência de um diretório é basicamente perguntar ao sistema operacional se um certo caminho no seu disco possui uma pasta. Programadores fazem isso para evitar erros ao tentar acessar, ler ou escrever em pastas que não existem.

## Como Fazer:
```javascript
const fs = require('fs');

// Síncrono
const dirPath = './meuDiretorio';
if (fs.existsSync(dirPath)) {
  console.log(`O diretório '${dirPath}' existe!`);
} else {
  console.log(`O diretório '${dirPath}' não existe.`);
}

// Assíncrono com async/await
const fsPromises = fs.promises;
async function checkDirectoryExists() {
  try {
    await fsPromises.access(dirPath, fs.constants.F_OK);
    console.log(`O diretório '${dirPath}' existe!`);
  } catch {
    console.log(`O diretório '${dirPath}' não existe.`);
  }
}

checkDirectoryExists();
```
Saída esperada (dependendo se o diretório existe ou não):
```
O diretório './meuDiretorio' existe!
```
ou
```
O diretório './meuDiretorio' não existe.
```

## Mergulho Profundo:
Historicamente, a necessidade de verificar a presença de um diretório antes de realizar operações sobre ele é vital para prevenir erros de 'diretório não encontrado'. Antes do Node.js, essa operação podia ser mais desafiadora em JavaScript, visto que era primariamente uma linguagem focada no navegador. Alternativas de checagem incluem usar a função `fs.stat` ou `fs.readdir`, que podem fornecer mais informações sobre o caminho em questão. Ao nível de implementação, a função `fs.existsSync` bloqueia a thread — ou seja, espera pela resposta antes de seguir adiante — enquanto `fsPromises.access` permite um approach não-bloqueante, ideal para aplicativos que precisam manter a fluidez de execução.

## Veja Também:
- Documentação Node.js fs: https://nodejs.org/api/fs.html
- Artigo sobre leitura e escrita de arquivos em Node.js: https://nodejs.dev/learn/reading-files-with-nodejs
- Explicações sobre programação assíncrona em JavaScript: https://developer.mozilla.org/pt-BR/docs/Learn/JavaScript/Asynchronous
