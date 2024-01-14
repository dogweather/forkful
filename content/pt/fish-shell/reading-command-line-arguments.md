---
title:    "Fish Shell: Lendo argumentos da linha de comando"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que utilizar argumentos de linha de comando?

Argumentos de linha de comando são uma forma eficiente e poderosa de interagir com o Fish Shell. Eles permitem que você passe informações e parâmetros para um programa diretamente da linha de comando, em vez de precisar interagir com uma interface gráfica ou editar um arquivo de configuração. Isso pode economizar tempo e agilizar tarefas repetitivas.

## Como utilizar argumentos de linha de comando no Fish Shell
Para utilizar argumentos de linha de comando no Fish Shell, você precisa seguir alguns passos simples:

1. Abra o Fish Shell.
2. Digite o comando desejado, seguido de seus argumentos separados por espaço.
3. Pressione Enter para executar o comando.

Por exemplo:

```Fish Shell
mkdir pasta-teste
```

Neste exemplo, "mkdir" é o comando para criar uma nova pasta e "pasta-teste" é o argumento passado para o comando.

Você também pode passar vários argumentos para um comando, como por exemplo:

```Fish Shell
cp arquivo.txt pasta-teste arquivo-copia.txt
```

Neste caso, o comando "cp" é utilizado para copiar o arquivo "arquivo.txt" para a pasta "pasta-teste" com o nome "arquivo-copia.txt".

## Mergulho aprofundado em argumentos de linha de comando
Ao utilizar argumentos de linha de comando, é importante entender como eles são interpretados pelo sistema operacional. Eles são básicamente uma forma de passar informações de entrada para um programa, assim como você faria ao clicar em botões em uma interface gráfica.

Ao utilizar o Fish Shell, você pode utilizar comandos especiais para manipular os argumentos de linha de comando, como por exemplo "$argv", que retorna todos os argumentos passados para o comando atual. Você também pode utilizar "$argc", que retorna o número total de argumentos.

Outra técnica útil é utilizar "for" loops para percorrer e manipular cada argumento individualmente. Por exemplo:

```Fish Shell
for arg in $argv
    echo "O argumento $arg possui $(echo $arg | wc -c | cut -d ' ' -f 1) caracteres."
end
```

Neste exemplo, o loop for percorre cada argumento e utiliza comandos como "echo", "wc" e "cut" para contar o número de caracteres em cada um. Em um caso real, você poderia utilizar essas informações para validar ou processar os argumentos antes de utilizá-los em um comando.

## Veja também
- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/)
- [Tutorial de argumentos de linha de comando no Fish Shell](https://fishshell.com/docs/current/tutorial.html#tut_arguments)
- [Exemplos práticos de utilização de argumentos de linha de comando](https://www.linux.com/blog/15-examples-fish-command-line-fishshell/)