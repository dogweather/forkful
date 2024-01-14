---
title:                "Javascript: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Ao programar em JavaScript, uma das tarefas comuns é verificar se um diretório existe antes de acessá-lo ou realizar qualquer operação nele. Isso é importante porque, ao tentar acessar um diretório inexistente, seu código pode falhar ou gerar erros desnecessários. Por isso, aprender a verificar se um diretório existe é uma habilidade valiosa para qualquer programador.

## Como fazer isso

Existem algumas maneiras de verificar se um diretório existe em JavaScript, mas a mais comum é usando a biblioteca "fs" (file system) do Node.js. Vamos mostrar como fazer isso usando essa biblioteca:

```JavaScript
// importando a biblioteca fs
const fs = require('fs');

// definindo o caminho do diretório a ser verificado
const directory = './meu_diretorio';

// verificando se o arquivo existe
fs.existsSync(directory, (exists) => {
    if (exists) {
        console.log('O diretório existe!');
    } else {
        console.log('O diretório não existe!');
    }
});
```

Nesse exemplo, importamos a biblioteca "fs" e usamos o método "existsSync" para verificar se o diretório definido na variável "directory" existe. Se existir, a mensagem "O diretório existe!" será impressa no console. Caso contrário, a mensagem "O diretório não existe!" será exibida.

## Aprofundando

Além do método "existsSync", a biblioteca "fs" também possui outras funções para verificar a existência de um diretório, como o "stat" e o "access". Essas funções permitem verificar se um arquivo ou diretório existe e se você possui permissões de leitura, escrita ou execução nele. Além disso, é possível trabalhar com tratamento de erros para lidar com situações onde o diretório pode não existir ou você não possui permissão para acessá-lo.

## Veja também

- [Documentação do Node.js sobre a biblioteca fs](https://nodejs.org/api/fs.html)
- [Artigo da Medium sobre verificação de arquivos e diretórios em Node.js](https://medium.com/@eugenioclrc/verificando-a-exist%C3%AAncia-de-arquivos-e-diret%C3%B3rios-com-nodejs-3a5d34fe05e3)
- [Stack Overflow: Como verificar se um arquivo ou diretório existe em Node.js](https://stackoverflow.com/questions/4482686/check-synchronously-if-file-directory-exists-in-node-js)