---
title:                "Verificando se um diretório existe."
html_title:           "TypeScript: Verificando se um diretório existe."
simple_title:         "Verificando se um diretório existe."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que
Você já se perguntou como o seu programa em TypeScript pode verificar se um diretório existe? Esta pode ser uma tarefa comum, mas ainda é um ponto importante a se considerar ao planejar seu código. Portanto, neste artigo, vamos explorar por que é importante verificar a existência de diretórios e como podemos fazer isso usando TypeScript.

## Como fazer
Para verificar se um diretório existe no TypeScript, podemos usar o módulo de sistema `fs`. Primeiro, precisamos importar esse módulo em nosso código, da seguinte forma:

```TypeScript
import * as fs from 'fs';
```

Em seguida, podemos usar o método `existsSync()` do módulo `fs` para verificar se um diretório existe. Este método retorna um valor booleano que indica se o diretório existe ou não. Vejamos um exemplo de código:

```TypeScript
const arquivo = 'caminho/para/diretório';
if (fs.existsSync(arquivo)) {
    console.log('O diretório existe.');
} else {
    console.log('O diretório não existe.');
}
```

Se o diretório existir, o código imprimirá "O diretório existe.". Caso contrário, imprimirá "O diretório não existe.".

## Mergulho Profundo
O método `existsSync()` é síncrono, o que significa que ele bloqueia a execução até que a verificação seja concluída. Existem também métodos assíncronos disponíveis, como o `exists()`, que usa o callback para retornar o resultado da verificação.

Além disso, podemos usar o módulo `path` para manipular o caminho do diretório de forma mais eficiente, permitindo que nosso código seja mais flexível e seja executado em diferentes sistemas operacionais.

## Veja também
- Documentação oficial do módulo `fs` do Node.js: https://nodejs.org/api/fs.html
- Documentação oficial do módulo `path` do Node.js: https://nodejs.org/api/path.html