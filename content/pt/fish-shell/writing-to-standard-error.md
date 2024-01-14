---
title:                "Fish Shell: Escrevendo no Erro Padrão"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que usar a saída de erro padrão em programação?

A saída de erro padrão é uma ferramenta útil para programadores porque permite que erros e mensagens de aviso sejam exibidos em tempo de execução. Isso é especialmente importante ao lidar com grandes quantidades de código, pois é uma forma de rastrear e encontrar erros rapidamente.

## Como usar a saída de erro padrão no Fish Shell

Para utilizar a saída de erro padrão no Fish Shell, basta adicionar o comando `echo` seguido do texto que deseja exibir entre aspas. Por exemplo:

```Fish Shell
echo "Oops, parece que algo deu errado!"
```

Isso resultaria na seguinte saída:

```
Oops, parece que algo deu errado!
```

Você também pode usar a variável de ambiente `stderr` para redirecionar a saída de erro para um arquivo em vez de exibi-la no terminal. Por exemplo:

```Fish Shell
echo "Oops, parece que algo deu errado!" 2 > stderr.txt
```

Isso criaria um arquivo chamado "stderr.txt" contendo a mensagem de erro.

## Mergulho profundo: Escrevendo para a saída de erro padrão

Além de simplesmente exibir mensagens de erro, você também pode utilizar a saída de erro padrão para fins de depuração. Por exemplo, você pode exibir o valor de uma variável utilizando o comando `set` combinado com `echo`:

```Fish Shell
set foo "bar"
echo $foo
```

Isso exibiria o valor da variável "foo" na saída de erro. Isso pode ser útil para identificar problemas em partes específicas do seu código.

Também é possível redirecionar a saída de erro para outro processo em vez de para um arquivo. Por exemplo, utilizando o comando `|` para conectar o resultado de um comando à entrada de outro comando:

```Fish Shell
ls | grep "arquivos"
```

Isso executaria o comando "ls" e em seguida encaminharia sua saída de erro para o comando "grep", que filtraria os arquivos contendo a palavra "arquivos". Esse é um exemplo simples, mas essa técnica pode ser útil para depurar scripts mais complexos.

## Veja também

- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/)
- [Artigo sobre redirecionamento de saída no Fish Shell](https://www.maketecheasier.com/redirect-output-fish-shell/)
- [Tutorial sobre depuração de código no Fish Shell](https://tech.teamed.io/articles/51-depth-guide-into-debugging-fish-shell-scripts/)