---
title:                "Verificando se um diretório existe"
html_title:           "Kotlin: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Verificando se um diretório existe em TypeScript

## O quê e por quê?

Em programação, "verificar se um diretório existe" é o ato de verificar a existência de um diretório específico dentro do sistema de arquivos. Programadores fazem isso para evitar erros ao tentar acessar ou manipular um diretório que pode não existir.

## Como fazer:

Vamos usar a biblioteca **fs** do Node.js para verificar a existência de um diretório. Aqui está o código em TypeScript:

```TypeScript
import * as fs from 'fs';

let diretorio: string = '/caminho/para/o/diretório';

fs.access(diretorio, fs.constants.F_OK, (err) => {
    if (err) {
        console.error(`${diretorio} não existe`);
    } else {
        console.log(`${diretorio} existe`);
    }
});
```

O código acima verificará se o diretório especificado existe e imprimirá a mensagem apropriada.

## Mergulho profundo:

Historicamente, podemos usar o método `fs.exists()` para verificar a existência de um diretório. No entanto, `fs.exists()` foi depreciado desde Node.js v4.0.0 devido à maneira desajeitada de lidar com erros e recomenda-se a utilização do método `fs.access()`.

Alternativamente, podemos usar o método síncrono `fs.existsSync()`:

```TypeScript
import * as fs from 'fs';

let diretorio: string = '/caminho/para/o/diretório';

if (fs.existsSync(diretorio)) {
    console.log(`${diretorio} existe`);
} else {
    console.error(`${diretorio} não existe`);
}
```

Observação importante: conveniente por ser síncrono, o `fs.existsSync()` pode bloquear o thread principal se o diretório estiver em um sistema de arquivos lento, causando potenciais problemas de desempenho.

## Veja também:

Para uma compreensão mais profunda dos métodos do sistema de arquivos e da programação de Node.js, você pode consulte esses recursos:

1. Documentação oficial do Node.js: [File System](https://nodejs.org/api/fs.html)
2. Guia de Início Rápido do TypeScript: [TypeScript em 5 minutos](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes.html)