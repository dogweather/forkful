---
title:                "Fish Shell: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que

Se você é um desenvolvedor que trabalha com datas, é provável que, em algum momento, precise compará-las em seu código. Isso pode ser necessário para verificar se uma data é maior ou menor do que outra, ou para verificar se elas são iguais.

## Como Fazer

A linguagem de programação Fish Shell oferece uma maneira simples e eficaz de comparar duas datas. Para fazer isso, basta utilizar o comando `date -f` e fornecer as datas a serem comparadas. Veja um exemplo abaixo:

```
Fish Shell
$ date -f %s '2021-01-01'
```

A saída desse comando será o formato de data Unix para a data fornecida, que é equivalente a `1609459200`. Para comparar duas datas, basta utilizar o operador de comparação `-lt` para "menor que", ou `-gt` para "maior que". Veja um exemplo:

```
```
Fish Shell
```
```

$ if date -f %s '2021-01-01' -lt date -f %s '2021-02-01'
    echo "A primeira data é menor que a segunda"
end
```

Na linha 3 do exemplo acima, o programa verifica se a primeira data é menor que a segunda, e, se for o caso, a mensagem "A primeira data é menor que a segunda" será impressa na tela.

## Mergulho Profundo
A comparação de datas pode ser feita de diferentes maneiras, dependendo da precisão que você deseja. Por exemplo, no exemplo acima, as datas foram comparadas utilizando o formato de data Unix. No entanto, você também pode compará-las utilizando o comando `date` e especificando o formato desejado. Veja um exemplo abaixo:

```
Fish Shell
$ if date -f "%b %Y" 'Jan 2021' -lt date -f "%b %Y" 'Feb 2021'
    echo "A primeira data é menor que a segunda"
end
```

Nesse exemplo, as datas foram comparadas utilizando o formato de mês e ano, que resultará em um resultado mais preciso.

## Veja Também
- [Documentação oficial do comando date](https://fishshell.com/docs/current/cmds/date.html)
- [Tutorial em vídeo sobre comparação de datas no Fish Shell](https://www.youtube.com/watch?v=6jLDZlbnx9I)
- [Exemplos avançados de comparações de datas no Fish Shell](https://hyperpolyglot.org/unix-shells#date-comparison)