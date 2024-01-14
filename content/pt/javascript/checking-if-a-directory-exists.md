---
title:    "Javascript: Verificando se um diretório existe"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Ao escrever códigos em Javascript, é muito comum lidar com diretórios. Verificar se um diretório existe é uma tarefa importante, pois permite que o programa execute determinadas ações com base na existência ou não dessa pasta. Por exemplo, você pode querer verificar se um diretório existe antes de tentar abri-lo ou criar um novo arquivo dentro dele.

## Como fazer isso?

Para verificar se um diretório existe em Javascript, podemos usar o módulo "fs" (file system) incorporado na linguagem. Este módulo fornece várias funções úteis para lidar com arquivos e diretórios. Neste caso, precisaremos usar a função "fs.existsSync()" que aceita o caminho do diretório como parâmetro e retorna um valor booleano, indicando se o diretório existe ou não.

Aqui está um exemplo de como podemos usar isso em nosso código:

```Javascript
const fs = require('fs');
const directory = './meu_diretorio';

if (fs.existsSync(directory)){
    console.log("O diretório existe!");
} else {
    console.log("O diretório não existe!");
}
```

Neste exemplo, estamos primeiro importando o módulo "fs". Em seguida, definimos o caminho do diretório que queremos verificar em uma variável. Em seguida, utilizamos a função "fs.existsSync()" dentro de uma estrutura condicional "if/else" para imprimir uma mensagem dependendo do retorno da função. Caso o diretório exista, o console irá imprimir "O diretório existe!", caso contrário, irá imprimir "O diretório não existe!".

Podemos usar a mesma lógica para fazer outras ações dentro de nosso programa, dependendo do resultado da verificação do diretório.

## Investigando mais a fundo

Além da função "fs.existsSync()", existem outras maneiras de verificar a existência de um diretório em Javascript, como por exemplo utilizar a função "fs.statSync()" que retorna informações sobre o diretório, incluindo se ele existe ou não. Além disso, também podemos usar a biblioteca externa "node-dir-exists" que oferece uma maneira mais simplificada de verificar a existência de um diretório.

## Veja também
- [Documentação do módulo fs](https://nodejs.org/api/fs.html)
- [Biblioteca node-dir-exists](https://www.npmjs.com/package/node-dir-exists)