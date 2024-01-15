---
title:                "Lendo argumentos da linha de comando"
html_title:           "Fish Shell: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando

Se você está familiarizado com a programação em shell, provavelmente já ouviu falar sobre a leitura de argumentos da linha de comando. Mas por que essa prática é importante? Bem, ler esses argumentos permite que seu código seja mais dinâmico e adaptável às informações inseridas pelo usuário, tornando-o mais eficiente e fácil de usar.

## Como fazer

Para ler argumentos da linha de comando em Fish Shell, primeiro você precisa definir uma variável que irá armazenar esses argumentos, usando o comando `set`. Por exemplo, se você quiser ler três argumentos, pode usar o seguinte código:

```Fish Shell
set arg1 $argv[1]
set arg2 $argv[2]
set arg3 $argv[3]
```

A variável `$argv` armazena todos os argumentos passados na linha de comando, e cada argumento é acessado usando um índice, começando em 1. Então, o primeiro argumento estará armazenado em `$argv[1]`, o segundo em `$argv[2]` e assim por diante.

Depois de definir as variáveis com os argumentos, você pode usá-las em seu código da maneira que quiser. Por exemplo, se o usuário digitar um nome de arquivo como argumento, você pode usar a variável correspondente para abrir esse arquivo e fazer operações nele.

## Mergulho profundo

Há algumas coisas a serem consideradas quando se trata de ler argumentos da linha de comando em Fish Shell. Um detalhe importante é que o primeiro argumento, `$argv[1]`, é sempre o nome do próprio script que está sendo executado. Isso significa que os argumentos inseridos pelo usuário começam a partir de `$argv[2]`.

Além disso, é importante saber que o valor de `$argv[1]` pode variar dependendo de como o script é executado. Por exemplo, se o usuário especificar o caminho completo do script (por exemplo, `/home/user/script.fish`), o valor de `$argv[1]` será esse caminho. Mas se o usuário executar o script a partir do diretório atual, usando apenas o nome (por exemplo, `./script.fish`), o valor de `$argv[1]` será apenas o nome do script.

Você também pode usar a opção `-h` como argumento para exibir uma ajuda ou descrição do script quando ele é executado. Isso pode ser feito verificando se o valor de `$argv[1]` é igual a `-h` e, em seguida, exibindo o texto de ajuda desejado.

## Veja também

- [Tutorial de Fish Shell (em inglês)](https://fishshell.com/docs/current/tutorial.html)
- [Documentação oficial de Fish Shell (em inglês)](https://fishshell.com/docs/current/index.html)