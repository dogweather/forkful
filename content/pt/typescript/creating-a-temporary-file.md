---
title:                "Criando um arquivo temporário"
html_title:           "Bash: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# TypeScript: Criando Arquivos Temporários

## O Que & Por Que?

Crear um arquivo temporário é uma forma rápida de armazenar dados de curto prazo que não precisam persistir além da sessão atual. Programadores geralmente criam arquivos temporários para gerenciar uma grande quantidade de dados que seriam consumir demais a memória se mantidos na RAM.

## Como Fazer:

Vamos fazer isso usando a biblioteca `tmp-promise`:

```TypeScript
import tmp from 'tmp-promise';

const example = async () => {
  const { path, fd, cleanup } = await tmp.file();
  console.log(path); // imprime o caminho do arquivo temporário no console
  // use 'cleanup' quando terminar de usar o arquivo para limpar o arquivo temporário
}

example();
```
## Deep Dive

Historicamente, arquivos temporários foram criados em linguagens de programação de baixo nível escrevendo no diretório do sistema apropriado (como `/tmp` em sistemas Unix). No TypeScript, as bibliotecas nos proporcionam uma maneira mais fácil e segura de gerenciar esses arquivos.

Como alternativa à biblioteca `tmp-promise`, você também pode usar a `fs` nativa do NodeJS para criar arquivos temporários, no entanto, a gestão deles é um pouco mais complicada.

A implementação em `tmp-promise` é baseada em promessas, assim como o restante do ecossistema moderno do JavaScript/TypeScript. Note que você deve chamar a função `cleanup` quando terminar de usar o arquivo temporário para evitar o vazamento de memória.

## Veja Também: 

1. Documentação oficial do `tmp-promise`: https://github.com/benjamingr/tmp-promise
2. Módulo filesystem (`fs`) do NodeJS: https://nodejs.dev/learn/the-nodejs-fs-module
3. Mais sobre arquivos temporários: https://en.wikipedia.org/wiki/Temporary_file