---
title:                "Fish Shell: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que

Se você é um programador ou está interessado em melhorar suas habilidades em linha de comando, entender como ler argumentos de linha de comando é essencial. Isso permite que você crie scripts mais dinâmicos e interativos que possam receber informações dos usuários durante a execução.

## Como Fazer

Para ler argumentos de linha de comando no Fish Shell, você pode usar o comando `arg` seguido do número do argumento que deseja ler. Por exemplo:

```
Fish Shell arg 1
```

Este comando irá exibir o primeiro argumento passado para o script. Você também pode usar `$argv` para acessar todos os argumentos passados de uma só vez.

Além disso, você pode usar a opção `-c` seguida de um número para ler argumentos a partir daquele número. Por exemplo:

```
Fish Shell arg -c 2
```

Isso exibirá todos os argumentos a partir do segundo.

## Mergulho Profundo

Ao ler argumentos de linha de comando no Fish Shell, é importante estar ciente de que esses argumentos são armazenados como uma lista de strings. Isso significa que, se um argumento contiver espaços ou caracteres especiais, ele será tratado como uma única string e não será dividido em argumentos separados.

Você também pode usar o comando `count $argv` para verificar quantos argumentos foram passados para o script. Isso pode ser útil para validar a entrada do usuário e evitar erros.

## Veja Também

- [Documentação do Fish Shell sobre os comandos arg e $argv](https://fishshell.com/docs/current/cmds/arg.html)
- [Tutorial sobre como ler argumentos de linha de comando no Fish Shell](https://dev.to/omarhakim/read-command-line-arguments-in-fish-shell-3poly)
- [Exemplos práticos de uso de argumentos de linha de comando no Fish Shell](https://www.learnshell.org/en/Command_Line_Arguments)