---
title:                "Criando um arquivo temporário"
html_title:           "Bash: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Criando um arquivo temporário em Javascript

## O Que e Por Que?

Criar um arquivo temporário significa gerar um arquivo para uso temporário. Programadores fazem isso quando precisam armazenar dados temporariamente, como durante testes unitários ou operações de manipulação de arquivo.

## Como fazer:

```Javascript
const fs = require('fs');
const tmp = require('tmp');

// Cria um arquivo temporário
let tmpobj = tmp.fileSync();
console.log('Arquivo Temporário Criado: ', tmpobj.name);

// Grava dados no arquivo temporário
fs.writeSync(tmpobj.fd, 'Hello, mundo!');
```

Quando você executa esse código, recebe uma saída assim:

```
Arquivo Temporário Criado: /tmp/1234567890abcdef.tmp
```

Então você vai encontrar o texto "Hello, mundo!" dentro do arquivo `/tmp/1234567890abcdef.tmp`.

## Mergulho Profundo

Criar arquivos temporários era comum nas épocas pre-internet, quando o armazenamento de dados era limitado e a operação de E/S de arquivo era cara. Agora, a criação de arquivos temporários ainda é útil para teste e operações temporárias de manipulação de arquivo.

Existem outras alternativas para realizar operações semelhantes. Por exemplo, Javascript permite armazenar dados na memória usando variáveis. No entanto, o arquivo temporário é preferível quando os dados precisam ser persistentes ou quando você está trabalhando com grandes quantidades de dados.

Quando você usa o pacote 'tmp' no Javascript, os arquivos temporários que você cria são excluídos quando o processo do Node.js é encerrado. Isto é, os arquivos que criamos acima são excluídos automaticamente.

## Veja Também

Para aprender mais sobre a criação de arquivos temporários, confira os seguintes links:
- [Pacote npm 'tmp'](https://www.npmjs.com/package/tmp)
- [Guia Node.js para E/S de arquivos](https://nodejs.dev/learn/the-nodejs-fs-module)
- [Tutorial Mdn sobre a leitura de arquivos em Javascript](https://developer.mozilla.org/en-US/docs/Web/API/File/Using_files_from_web_applications)