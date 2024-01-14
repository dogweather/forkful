---
title:    "Fish Shell: Obtendo a data atual"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Por que?

Se você é um programador iniciante, pode ficar se perguntando por que precisaria obter a data atual durante a programação. Bem, há várias razões para isso. Por exemplo, você pode precisar registrar a data em que um arquivo foi criado, ou então usar a data atual como parte do seu código para fins de organização ou registro.

## Como fazer:

Felizmente, com o Fish Shell, obter a data atual é uma tarefa simples e direta. Basta seguir esses passos:

1. Abra o terminal e inicie o Fish Shell usando o comando `fish`.
2. Para obter a data atual, use o comando `date +%d/%m/%Y` que irá retornar a data no formato "dia/mês/ano".

Aqui está um exemplo de como esse comando seria usado no Fish Shell:

```
Fish Shell > date +%d/%m/%Y
13/09/2021
```
Note que o formato da data pode ser alterado de acordo com suas necessidades. Por exemplo, se você quiser incluir o horário, pode usar `date +%d/%m/%Y %H:%M:%S`.

## Mergulhando mais fundo:

Além de retornar a data atual, o Fish Shell também oferece a opção de adicionar ou subtrair valores da data. Por exemplo, se você quiser obter a data daqui a uma semana, pode fazer isso usando `date -d "+7 days" +%d/%m/%Y`. Você também pode personalizar a data usando outros parâmetros, como "years" (anos) ou "months" (meses).

Outra funcionalidade interessante do Fish Shell é a capacidade de converter a data para outros formatos, como o Unix Timestamp. Você pode fazer isso usando o comando `date -j +%s` que irá retornar o timestamp atual.

## Veja também:

- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/index.html)
- [Guia de referência para comandos do Fish Shell](https://fishshell.com/docs/current/cmds.html)
- [Tópico do Stack Overflow sobre como obter a data atual no Fish Shell](https://stackoverflow.com/questions/24150486/get-current-date-and-time-in-fish-shell)