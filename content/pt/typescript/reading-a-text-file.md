---
title:                "Lendo um arquivo de texto"
html_title:           "Bash: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

---

# Lendo um ficheiro de texto com TypeScript

## O que & Por quê?

Ler um ficheiro de texto é o processo de obter dados digitais em formato de texto a partir de um ficheiro. Os programadores fazem isso para manipular ou aproveitar informações já existentes sem ter que hardcoded-las no programa.

## Como fazer:

A biblioteca nativa `fs (file system)` de Node.js nos permite ler um ficheiro de texto de maneira simples e eficiente.

```TypeScript
import fs from 'fs';

fs.readFile('meuficheiro.txt', 'utf8' , (err, data) => {
  if (err) {
    console.error(err);
    return;
  }
  console.log(data);
})
```

Quando corre esse código, ele vai ler o conteúdo do ficheiro `meuficheiro.txt` e vai imprimir na consola.

## Deep Dive

Historicamente, a leitura de ficheiros em Javascript era um pouco incômoda devido ao seu modelo de thread única. No entanto, com a introdução de Node.js e o seu ambiente assíncrono, essa tarefa se tornou muito mais simples e eficiente.

Existem outras alternativas como a biblioteca `readline` que permite ler ficheiros de texto de forma linear, linha por linha, tornando útil para ficheiros grandes que não cabem na memória. 

Por trás das cortinas, a função `readFile` do módulo `fs` usa uma implementação do sistema de arquivos POSIX que é muito eficiente em ambientes Unix-like e também em Windows.

## Ver também

1. Documentação oficial do Node.js [`fs` module](https://nodejs.org/api/fs.html)
2. [`readline module`](https://nodejs.org/api/readline.html) - se necessita ler ficheiros grandes linha por linha 
3. Documentação oficial do TypeScript [TypeScript in Node.js](https://nodejs.org/api/fs.html) - para compreender melhor como utilizar TypeScript com o Node.js
4. [POSIX](https://pt.wikipedia.org/wiki/POSIX) - para uma visão mais profunda do que está a acontecer por trás do código

---