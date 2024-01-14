---
title:                "TypeScript: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

A verificação da existência de um diretório é necessária em diversas situações de programação. Por exemplo, quando se deseja criar um novo diretório, é importante saber se ele já existe para evitar conflitos e erros. Além disso, a verificação de diretórios também pode ser útil para garantir que um determinado caminho de diretório existe antes de realizar alguma operação com ele.

## Como fazer a verificação em TypeScript

Para fazer a verificação de um diretório em TypeScript, podemos utilizar a função "fs.existsSync()" do módulo "fs". Veja um exemplo abaixo:

```TypeScript
import * as fs from 'fs';

// Verificação de um diretório existente
const directoryPath = './diretorio';
if (fs.existsSync(directoryPath)) {
    console.log('O diretório existe!');
} else {
    console.log('O diretório não existe!');
}

// Verificação de um diretório inexistente
const directoryPath = './diretorio_inexistente';
if (fs.existsSync(directoryPath)) {
    console.log('O diretório existe!');
} else {
    console.log('O diretório não existe!');
}

```

A saída para esse código será:

```
O diretório existe!
O diretório não existe!
```

## Como funciona a verificação de diretórios

A função "fs.existsSync()" retorna "true" caso o diretório exista e "false" caso não exista. Ela recebe como parâmetro o caminho do diretório que se deseja verificar. Além disso, também é possível fazer a verificação de arquivos utilizando a função "fs.existsSync()".

## Veja também

- Documentação oficial do módulo "fs": https://nodejs.org/api/fs.html
- Exemplo de verificação de arquivos em TypeScript: https://stackoverflow.com/questions/40599583/typescript-verify-if-folder-exists