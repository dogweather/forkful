---
title:                "Fish Shell: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto com Fish Shell

Se você é um programador ou simplesmente gosta de brincar com o Fish Shell, pode se perguntar por que alguém iria querer ler um arquivo de texto no terminal. A resposta é simples: ler um arquivo de texto pode facilitar a realização de tarefas repetitivas ou automatizar processos em massa.

## Como fazer isso com o Fish Shell

Para ler um arquivo de texto com o Fish Shell, primeiro precisamos usar o comando `cat`. Este comando é usado para exibir o conteúdo de um arquivo de texto em sua saída padrão.

```Fish Shell
cat arquivo.txt
```

Com isso, o conteúdo do arquivo de texto será exibido no seu terminal. Você também pode usar o comando `head` para exibir apenas as primeiras linhas do arquivo ou `tail` para exibir as últimas linhas.

```Fish Shell
head arquivo.txt
tail arquivo.txt
```

Você também pode usar o comando `grep` para encontrar palavras ou padrões específicos no arquivo de texto.

```Fish Shell
grep "palavra" arquivo.txt
```

## Aprofundando um pouco mais

Ao ler um arquivo de texto com o Fish Shell, ele será lido linha por linha, o que significa que podemos usar um loop para executar ações em cada linha. Veja o exemplo abaixo:

```Fish Shell
while read linha
  echo $linha
end < arquivo.txt
```

Isso exibirá na saída padrão cada linha do arquivo de texto. Além disso, você pode usar comandos de redirecionamento para salvar o resultado da leitura em um novo arquivo, por exemplo:

```Fish Shell
grep "palavra" arquivo.txt > resultado.txt
```

## Veja também

- Aprenda mais sobre o Fish Shell: [https://fishshell.com/](https://fishshell.com/)
- Documentação do comando `cat`: [https://fishshell.com/docs/current/cmds/cat.html](https://fishshell.com/docs/current/cmds/cat.html)
- Saiba mais sobre loops no Fish Shell: [https://fishshell.com/docs/current/tutorial.html#tut_loops](https://fishshell.com/docs/current/tutorial.html#tut_loops)