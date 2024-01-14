---
title:                "Javascript: Verificando se um diretório existe"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que?

Verificar se um diretório existe é uma tarefa essencial para garantir que seu código funcione corretamente. Isso é especialmente importante ao trabalhar com manipulação de arquivos e pastas em um aplicativo Javascript. Neste blog post, mostraremos como verificar se um diretório existe usando código Javascript e mergulhar mais fundo no processo.

## Como Fazer?

Aqui estão alguns exemplos de código utilizando a função "fs.exists" do Node.js para verificar se um diretório existe:

```Javascript
const fs = require('fs'); // importa o módulo File System

// função que verifica se um diretório existe
function checkDirectory(directoryPath) {
  fs.exists(directoryPath, function(exists) { // chama a função "fs.exists" passando o caminho do diretório e uma função de retorno de chamada
    if (exists) { // se o diretório existir
      console.log("O diretório " + directoryPath + " existe!"); // imprime a mensagem indicando que o diretório existe
      return true; // retorna verdadeiro
    } else { // se o diretório não existir
      console.log("O diretório " + directoryPath + " não existe!"); // imprime a mensagem indicando que o diretório não existe
      return false; // retorna falso
    }
  });
}

// exemplo de chamada da função
checkDirectory('caminho/para/o/diretorio');

// saída no console: O diretório caminho/para/o/diretorio existe!
```

## Mergulho Profundo

Ao verificar se um diretório existe, é importante entender que a função "fs.exists" do Node.js verifica se um caminho existe, não um diretório específico. Isso significa que, se o caminho fornecido apontar para um arquivo em vez de um diretório, a função retornará "true" mesmo que o diretório não exista.

Para garantir que um diretório específico existe, podemos usar a função "fs.stat" do Node.js, que retorna informações sobre um arquivo ou diretório específico. Podemos então verificar o tipo de arquivo retornado usando a função "isDirectory()", que retorna verdadeiro ou falso com base no tipo de arquivo fornecido. Aqui está um exemplo de código:

```Javascript
const fs = require('fs');

// função que verifica se um diretório existe
function checkDirectory(directoryPath) {
  fs.stat(directoryPath, function(err, stats) { // chama a função "fs.stat" passando o caminho do diretório e uma função de retorno de chamada com dois argumentos: "err" para possíveis erros e "stats" para informações do arquivo/diretório
    if (err) { // se houver um erro
      console.log(err); // imprime o erro no console
      return false; // retorna falso
    }
    if (stats.isDirectory()) { // se o caminho fornecido for um diretório
      console.log("O diretório " + directoryPath + " existe!"); // imprime a mensagem indicando que o diretório existe
      return true; // retorna verdadeiro
    } else { // se o caminho fornecido não for um diretório
      console.log("O caminho fornecido não é um diretório válido!"); // imprime a mensagem indicando que o caminho fornecido não é um diretório válido
      return false; // retorna falso
    }
  });
}

// exemplo de chamada da função
checkDirectory('caminho/para/o/diretorio');

// saída no console: O diretório caminho/para/o/diretorio existe!
```

## Veja Também

- Tutorial do Node.js sobre fs.exists: https://nodejs.org/dist/latest-v12.x/docs/api/fs.html#fs_fs_exists_path_callback
- Tutorial do Node.js sobre fs.stat: https://nodejs.org/dist/latest-v12.x/docs/api/fs.html#fs_fs_stat_path_options_callback