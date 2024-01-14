---
title:                "Fish Shell: Comparando duas datas"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que Comparar Duas Datas em Fish Shell?

A tarefa de comparar duas datas pode surgir em diversas situações durante a programação. Pode ser necessário verificar se uma data é anterior ou posterior a outra, ou ainda calcular a diferença entre elas. O Fish Shell possui um mecanismo eficiente para realizar essas comparações, permitindo que o programador tenha mais controle sobre suas aplicações.

## Como Fazer a Comparação em Fish Shell?

Para realizar a comparação de duas datas em Fish Shell, é necessário seguir alguns passos simples:

1. Defina as duas datas a serem comparadas, utilizando o formato `YYYY-MM-DD`.
2. Utilize o operador `>` para verificar se a primeira data é posterior à segunda, ou `>=` para verificar se é posterior ou igual.
3. Utilize o operador `<` para verificar se a primeira data é anterior à segunda, ou `<=` para verificar se é anterior ou igual.

Veja o exemplo abaixo que compara duas datas e imprime a mensagem correspondente:

```Fish Shell
data1='2020-01-01'
data2='2020-02-01'

if [ $data1 > $data2 ]
    echo 'A primeira data é posterior à segunda'
else if [ $data1 >= $data2 ]
    echo 'A primeira data é posterior ou igual à segunda'
else if [ $data1 < $data2 ]
    echo 'A primeira data é anterior à segunda'
else if [ $data1 <= $data2 ]
    echo 'A primeira data é anterior ou igual à segunda'
end
```

O resultado dessa execução será:

```Fish Shell
A primeira data é anterior à segunda
```

## Mais Detalhes Sobre a Comparação de Datas em Fish Shell

Ao comparar duas datas em Fish Shell, é importante ter em mente que o formato utilizado deve ser sempre `YYYY-MM-DD`. Além disso, é possível realizar comparações com datas futuras, mas lembre-se que o resultado pode ser afetado pelo sistema de fuso horário do computador.

## Veja Também

- [Documentação do Fish Shell](https://fishshell.com/docs/current/)
- [Tutorial para Iniciantes em Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Comparando Dados em Shell Script](https://linux.die.net/abs-guide/moreadv.html#DATACOMPLICATED)