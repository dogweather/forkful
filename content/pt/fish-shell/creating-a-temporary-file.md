---
title:                "Criando um arquivo temporário"
html_title:           "Fish Shell: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O que e por que?

Criar um arquivo temporário é uma prática comum entre programadores. Basicamente, é um arquivo que é criado temporariamente para armazenar informações ou dados durante a execução de um programa. Os programadores geralmente o fazem para evitar sobrecarregar a memória do computador ou para evitar conflitos ao acessar determinados arquivos.

## Como fazer:

Para criar um arquivo temporário no Fish Shell, você pode usar o comando `mktemp`. Por exemplo:

```
Fish Shell $ mktemp
/tmp/tmp.7sbosSl3uh
```

Este comando criará um arquivo temporário com um nome aleatório no diretório `/tmp`. Você também pode especificar um prefixo para o nome do arquivo usando a opção `-p`. Por exemplo:

```
Fish Shell $ mktemp -p mytempfile
/tmp/mytempfile.esJYLsBM
```

Você também pode especificar o sufixo do nome do arquivo usando a opção `-s`. Por exemplo:

```
Fish Shell $ mktemp -s .log
/tmp/tmp.c7TGPXZ6yf.log
```

Além disso, você pode combinar as opções `-p` e `-s` para ter mais controle sobre o nome do arquivo temporário.

## Mergulho profundo:

A criação de arquivos temporários é uma técnica usada há muito tempo pelos programadores para lidar com a alocação de memória e garantir a segurança dos sistemas de arquivos. Existem outras maneiras de criar arquivos temporários, como usando a função `tempnam()` na linguagem C ou o módulo `tempfile` no Python. No entanto, o comando `mktemp` no Fish Shell é uma opção rápida e conveniente.

Ao usar o comando `mktemp`, é importante lembrar que o nome do arquivo temporário gerado pode ser acessado por outros usuários no sistema. Portanto, certifique-se de ajustar as permissões de acesso ao arquivo, se necessário.

## Veja também:

- [Documentação do Fish Shell](https://fishshell.com/docs/current/commands.html#mktemp)
- [Expansão de caracteres no Fish Shell](https://fishshell.com/docs/current/tutorial.html#tut_expansion)