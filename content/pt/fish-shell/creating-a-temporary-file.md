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

## Por que
Existem várias razões pelas quais alguém pode querer criar um arquivo temporário em um programa de shell, como por exemplo, armazenar dados temporários ou compartilhar informações entre diferentes execuções de comandos.

## Como fazer
A criação de um arquivo temporário é uma tarefa simples no Fish Shell. Basta seguir os seguintes passos:

1. Abra o terminal e execute o comando do Fish Shell.
2. Use o comando `mktemp` seguido de `> arquivo_temp` para criar um arquivo temporário com um nome aleatório.
3. Agora, você pode usar esse arquivo para armazenar dados temporários ou compartilhar informações.

Exemplo de código:

```Fish Shell
$ mktemp > arquivo_temp
```

Exemplo de saída:

`/var/folders/my/arquivo_temp`

## Aprofundando
O comando `mktemp` é responsável por criar o arquivo temporário com um nome aleatório. Por padrão, ele cria o arquivo no diretório `/tmp`, mas você pode especificar um diretório diferente usando a opção `-p` seguida do caminho desejado.

Uma opção útil do comando `mktemp` é a `-d`, que cria um diretório temporário em vez de um arquivo.

Para excluir o arquivo temporário criado, basta usar o comando `rm` seguido do nome do arquivo, ou usar a função `trap` para remover o arquivo automaticamente quando o programa terminar a execução.

## Veja também
- [Documentação do comando `mktemp`](https://fishshell.com/docs/current/commands.html#mktemp)
- [Tutorial: Como criar e excluir arquivos temporários no Linux](https://www.hostinger.com.br/tutoriais/linux-temporary-files)
- [Guia de referência do Fish Shell](https://fishshell.com/docs/current/index.html#references)