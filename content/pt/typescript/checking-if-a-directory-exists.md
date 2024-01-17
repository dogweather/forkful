---
title:                "Verificando se um diretório existe"
html_title:           "TypeScript: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

### O que e por que

Verificar se um diretório existe é uma tarefa comum em programação, especialmente ao lidar com arquivos e pastas. É basicamente um processo de validação para garantir que um determinado diretório existe antes de executar outras operações relacionadas a ele. Os programadores fazem isso para evitar possíveis erros ou falhas no código, garantindo que o caminho do diretório seja válido antes de continuar com suas tarefas.

### Como fazer

Em TypeScript, existem várias maneiras de verificar se um diretório existe. Aqui estão dois exemplos usando as funções nativas do Node.js:

```TypeScript
// Importando o módulo fs do Node.js para lidar com arquivos e diretórios
import * as fs from 'fs';

// Método síncrono fs.existsSync()
const directoryPath = './meu-diretorio';
if (fs.existsSync(directoryPath)) {
  console.log('O diretório existe!');
} else {
  console.log('O diretório não existe!');
}

// Método assíncrono fs.access() com a opção fs.constants.F_OK
const diretoryPath = './meu-diretorio';
fs.access(diretoryPath, fs.constants.F_OK, (error) => {
  if (error) {
    console.log('O diretório não existe!');
  } else {
    console.log('O diretório existe!');
  }
})

```

O método ```fs.existsSync()``` retorna um valor booleano, enquanto o método assíncrono ```fs.access()``` possui um parâmetro de retorno de chamada para lidar com resultados de forma assíncrona. Ambos os métodos aceitam o caminho do diretório como primeiro parâmetro e a opção ```fs.constants.F_OK``` para verificar a existência do diretório.

### Profundidade

A verificação de existência de diretórios é essencial em alguns cenários, principalmente ao trabalhar com arquivos e pastas em sistemas de arquivos. Em algumas linguagens de programação, como C++, existem funções específicas para verificar se um diretório existe. Em outras, como Java, é necessário manusear exceções ao tentar acessar um diretório inexistente. Em TypeScript, pode-se também usar a biblioteca externa [mkdirp](https://www.npmjs.com/package/mkdirp) para criar um diretório caso ele não exista ao invés de apenas verificar sua existência.

O Node.js usa as APIs do sistema operacional subjacente para verificar a existência de diretórios, portanto, depende da plataforma em que está sendo executado. No Windows, arquivos e diretórios com caracteres como `:` não são suportados, enquanto no Linux podem causar problemas ao criar caminhos e nomes de diretório.

### Veja também

- [Node.js - fs.existsSync()](https://nodejs.org/dist/latest-v14.x/docs/api/fs.html#fs_fsexistssync_path)
- [Node.js - fs.access()](https://nodejs.org/dist/latest-v14.x/docs/api/fs.html#fs_m_class_fs_fsaccesssync_path_mode)
- [mkdirp - npm](https://www.npmjs.com/package/mkdirp)