---
title:                "Criando um arquivo temporário"
html_title:           "Bash: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Criar um arquivo temporário no Bash é essencialmente criar um arquivo que será usado apenas temporariamente durante a execução de um programa. Programadores costumam criar arquivos temporários para armazenar dados temporários, realizar operações em arquivos existentes ou para fins de teste.

## Como fazer:

Para criar um arquivo temporário no Bash, podemos usar o comando `mktemp`. Por exemplo, podemos criar um arquivo temporário chamado "meutemp" usando o seguinte comando:

```Bash
mktemp -t meutemp
```

Isso criará um arquivo com um nome único, como "meutemp.XXXX" (onde "XXXX" é uma string aleatória). Podemos então usar esse arquivo para armazenar dados temporários e, quando não precisarmos mais dele, podemos excluí-lo usando o comando `rm`.

```Bash
rm meutemp.XXXX
```

## Profundando:

Criar arquivos temporários é uma prática comum em programação, especialmente em ambientes Unix e Linux. Antes do comando `mktemp` ser introduzido em 1999, os programadores costumavam usar nomes pré-definidos para seus arquivos temporários, o que poderia levar a conflitos se dois programas estivessem usando o mesmo nome. Com o comando `mktemp`, um nome único é gerado automaticamente, evitando esse problema.

Existem também outras formas de criar arquivos temporários no Bash, como usar o comando `touch` ou usar a variável de ambiente `$RANDOM`. Porém, o uso do comando `mktemp` é considerado o método mais seguro e recomendado.

## Veja também:

- [Documentação do comando `mktemp`](https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html)
- [Outras opções para criação de arquivos temporários em Bash](https://stackoverflow.com/questions/11910713/bash-how-to-make-a-random-file-name/11912421#11912421)