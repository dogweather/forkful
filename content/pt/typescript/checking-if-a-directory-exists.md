---
title:                "Verificando se um diretório existe"
date:                  2024-01-20T14:59:04.110037-07:00
html_title:           "Fish Shell: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Verificar a existência de um diretório é checar se uma pasta específica está presente no sistema de arquivos. Programadores fazem isso para evitar erros ao tentar acessar, ler ou escrever em diretórios que não existem.

## Como Fazer:
Usaremos o módulo `fs` do Node.js para verificar se um diretório existe. `fs.existsSync` retorna um booleano e `fs.promises.access` trabalha com promessas para operações assíncronas.

```typescript
import * as fs from 'fs';

// Método síncrono
const directoryExistsSync = (path: string): boolean => {
  return fs.existsSync(path);
}

console.log(directoryExistsSync('./minhaPasta')); // Saída: true ou false

// Método assíncrono com promessas
const directoryExistsAsync = async (path: string): Promise<boolean> => {
  try {
    await fs.promises.access(path, fs.constants.F_OK);
    return true;
  } catch {
    return false;
  }
}

directoryExistsAsync('./minhaPasta').then(exists => {
  console.log(exists); // Saída: true ou false
});
```

## Aprofundando o Conhecimento:
Historicamente, o módulo `fs` não tinha suporte para promessas, e as funções assíncronas utilizavam callbacks. Com a evolução do Node.js, foi introduzido `fs.promises` que permitiu o uso de `async/await`. Alternativamente, pode-se usar a função `fs.stat` ou `fs.access`, mas cuidado com potenciais problemas de race condition - quando o estado do arquivo muda entre a checagem e a operação subsequente. É importante entender a diferença entre métodos síncronos (bloqueantes) e assíncronos (não bloqueantes) para decidir qual é o melhor para a sua aplicação.

## Veja Também:
- Documentação Node.js `fs`: https://nodejs.org/api/fs.html
- Artigo sobre problemas de race conditions no SO: https://stackoverflow.com/questions/2739376/example-of-race-condition
- Blog sobre callbacks, promises e async/await: https://blog.risingstack.com/node-js-at-scale-understanding-node-js-event-loop/
