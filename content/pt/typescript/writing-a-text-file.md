---
title:                "Escrevendo um arquivo de texto"
html_title:           "Arduino: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Escrever um arquivo de texto é o processo de salvar dados em um formato legível no disco. Programadores fazem isso para persistir informações entre sessões, configurar sistemas ou exportar dados.

## Como Fazer:
Você vai usar o módulo `fs` do Node.js para escrever em arquivos. Primeiro, instale o Node, se ainda não o fez. Assuma que usamos a promessa baseada em `fs.promises`.

```typescript
import { writeFile } from 'fs/promises';

async function gravarArquivo() {
  try {
    await writeFile('mensagem.txt', 'Olá, mundo!');
    console.log('Arquivo gravado com sucesso.');
  } catch (erro) {
    console.error('Ocorreu um erro ao gravar o arquivo:', erro);
  }
}

gravarArquivo();
```

Saída esperada no console:
```
Arquivo gravado com sucesso.
```

## Aprofundando:
Historicamente, a escrita de arquivos em JavaScrit era uma operação apenas no lado do servidor. Com Node.js, isso se tornou padrão para automação e scripts de servidor. Alternativas incluem: APIs de armazenamento do navegador para aplicações web, bibliotecas como `lowdb` para JSON simples ou sistemas de banco de dados para armazenamento mais complexo. Detalhadamente, utilizar o `fs` diretamente é uma escolha de baixo nível. Para projetos maiores, considere ORM ou abstrações de arquivos como parte de um framework.

## Veja Também:
- Node.js `fs` module: https://nodejs.org/api/fs.html
- Documentação do TypeScript: https://www.typescriptlang.org/docs/
- Guia sobre promessas e async/await: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Using_promises